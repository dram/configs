#include <gio/gunixsocketaddress.h>

#include <fcitx/frontend.h>
#include <fcitx-gclient/fcitxclient.h>

#define EMACS_FCITX_SOCKET "/tmp/emacs-im-agent.socket"


static GString *committed;
static FcitxClient *client;


void connect_cb(FcitxClient *client, void *user_data)
{
	fcitx_client_set_capacity(client, CAPACITY_CLIENT_SIDE_UI);

	fcitx_client_focus_in(client);
	fcitx_client_enable_ic(client);
}

void destroy_cb(FcitxClient *client, void *user_data)
{
	puts("destroy_cb()");
}

void close_im_cb(FcitxClient *client, void *user_data)
{
	puts("close_im_cb()");
}

void commit_string_cb(FcitxClient *client, char *str, void *user_data)
{
	g_string_printf(committed, "\"%s\"", str);
}

void enable_im_cb(FcitxClient *client, void *user_data)
{
	puts("enable_im_cb()");
}

void forward_key_cb(FcitxClient *client,
		    guint keyval,
		    guint state,
		    gint type,
		    void *user_data)
{
	puts("forward_key_cb()");
}

void update_client_side_ui_cb(FcitxClient *client,
			      char *auxup,
			      char *auxdown,
			      char *preedit,
			      char *candidateword,
			      char *imname,
			      int cursorpos,
			      void *user_data)
{
	GString *data;
	GSocket *connection = user_data;

	data = g_string_new("");

	if (!committed->len)
		g_string_assign(committed, "nil");

	if (candidateword[0] == '1') {
		GRegex *regex;
		gchar *candidates;

		regex = g_regex_new(" \\d+\\.", 0, 0, NULL);

		candidateword += 2;
		candidateword[strlen(candidateword) - 1] = 0;

		candidates = g_regex_replace_literal(regex,
						     candidateword,
						     -1,
						     0,
						     "\" \"",
						     0,
						     NULL);

		g_string_printf(data, "'(\"%s\" (\"%s\") %s)",
				preedit, candidates, committed->str);

		g_free(candidates);
		g_free(regex);
	} else {
		g_string_printf(data, "'(\"%s\" () %s)",
				preedit, committed->str);
	}

	if (g_socket_send(connection,
			  data->str, data->len, NULL, NULL) == -1) {
		perror("g_socket_send");
		exit(1);
	}

	g_string_truncate(committed, 0);

	g_string_free(data, TRUE);
}

gboolean source_cb(GSocket *socket, GIOCondition condition, void *data)
{
	int sym;
	gchar chr;
	GMainLoop *main_loop = data;

	if (g_socket_receive(socket, &chr, 1, NULL, NULL) <= 0) {
		g_main_loop_quit(main_loop);
		return FALSE;
	}

	switch (chr) {
	case 13: sym = FcitxKey_Return; break;
	case 27: sym = FcitxKey_Escape; break;
	case 127: sym = FcitxKey_BackSpace; break;
	default: sym = chr; break;
	}

	fcitx_client_process_key_sync(client, sym, chr, 0, 0, 0);

	return TRUE;
}

GSocket *create_socket(void)
{
	GSocket *sock;
	GSocketAddress *addr;

	sock = g_socket_new(
		G_SOCKET_FAMILY_UNIX, G_SOCKET_TYPE_STREAM, 0, NULL);

	addr = g_unix_socket_address_new(EMACS_FCITX_SOCKET);

	unlink(EMACS_FCITX_SOCKET);

	g_socket_bind(sock, addr, FALSE, NULL);

	g_socket_listen(sock, NULL);

	return sock;
}

int main(void)
{
	GSocket *sock;

	sock = create_socket();
	committed = g_string_new("");

	puts("Emacs-fcitx is ready.");

	while (TRUE) {
		GSource *source;
		GSocket *connection;
		GMainLoop *main_loop;

		if ((connection = g_socket_accept(sock, NULL, NULL)) == NULL) {
			perror("g_socket_accept");
			exit(1);
		}

		puts("Opening connection.");

		client = fcitx_client_new();

		g_signal_connect(client, "connected",
				 G_CALLBACK(connect_cb), NULL);
		g_signal_connect(client, "disconnected",
				 G_CALLBACK(destroy_cb), NULL);
		g_signal_connect(client, "enable-im",
				 G_CALLBACK(enable_im_cb), NULL);
		g_signal_connect(client, "close-im",
				 G_CALLBACK(close_im_cb), NULL);
		g_signal_connect(client, "forward-key",
				 G_CALLBACK(forward_key_cb), NULL);
		g_signal_connect(client, "commit-string",
				 G_CALLBACK(commit_string_cb), NULL);
		g_signal_connect(client,
				 "update-client-side-ui",
				 G_CALLBACK(update_client_side_ui_cb),
				 connection);

		main_loop = g_main_loop_new(NULL, FALSE);

		source = g_socket_create_source(
			connection,
			(GIOCondition)(G_IO_IN | G_IO_HUP | G_IO_ERR), NULL);

		g_source_set_callback(
			source, (GSourceFunc)source_cb, main_loop, NULL);

		g_source_attach(source, NULL);

		g_main_loop_run(main_loop);

		puts("main loop finished.");

		g_socket_close(connection, NULL);

		g_object_unref(client);
		g_source_unref(source);
		g_object_unref(connection);
		g_main_loop_unref(main_loop);
	}

	g_object_unref(sock);
	g_string_free(committed, TRUE);

	return 0;
}

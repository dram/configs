#include <stdio.h>
#include <stdlib.h>

#include <string>
#include <iostream>

#include <unistd.h>

#include <sys/un.h>
#include <sys/socket.h>

#include <sunpinyin.h>

#define SUNPINYIN_SOCKET "/tmp/emacs-sunpinyin.socket"

std::string preedit;
std::string candidates;
std::string committed;

const char *mbs(const TWCHAR* wcs) {
	size_t bytes;
	const size_t max = 1024;
	static char str[max];

	bytes = WCSTOMBS(str, wcs, max);
	str[bytes] = 0;

	return str;
}

class WindowHandler : public CIMIWinHandler
{
public:
	virtual void commit(const TWCHAR* sstr);
	virtual void updateCandidates(const ICandidateList* pcl);
	virtual void updatePreedit(const IPreeditString* ppd);
	virtual void updateStatus(int key, int value);
};


void WindowHandler::commit(const TWCHAR* str)
{
	committed.clear();
	committed.push_back('"');
	committed.append(mbs(str));
	committed.push_back('"');
}

void WindowHandler::updateCandidates(const ICandidateList* pcl)
{
	candidates.clear();
	candidates.push_back('(');
        for (int i = 0; i < pcl->size(); i++) {
		candidates.push_back('"');
		candidates.append(mbs(pcl->candiString(i)));
		candidates.push_back('"');
        }
	candidates.push_back(')');
}

void WindowHandler::updatePreedit(const IPreeditString* ppd)
{
	preedit.clear();
	preedit.push_back('"');
	preedit.append(mbs(ppd->string()));
	preedit.push_back('"');
}

void WindowHandler::updateStatus(int key, int value) { }


int main(void) {
	int fd;
	struct sockaddr_un addr;

	CSunpinyinSessionFactory& factory = CSunpinyinSessionFactory::getFactory();

	if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
		perror("socket");
		exit(1);
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, SUNPINYIN_SOCKET, sizeof(SUNPINYIN_SOCKET));

	unlink(SUNPINYIN_SOCKET);

	if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) == -1) {
		perror("bind");
		exit(1);
	}

	if (listen(fd, 5) == -1) {
		perror("listen");
		exit(1);
	}

	printf("Emacs-sunpinyin is ready.\n");

	while (1) {
		int conn;
		char buf[1];
		CIMIView *view;
		WindowHandler *handler;

		if ((conn = accept(fd, NULL, NULL)) == -1) {
			perror("accept");
			exit(1);
		}

		printf("Opening connection.\n");

		view = factory.createSession();
		view->getIC()->setCharsetLevel(0); // 0: GB2312, 1: GBK
		handler = new WindowHandler();
		view->attachWinHandler(handler);

		while (read(conn, buf, sizeof(buf)) > 0) {
			int code;
			int chr = buf[0];
			std::string data;

			printf("code: %d\n", chr);

			switch (chr) {
			case 13: code = IM_VK_ENTER; break;
			case 27: code = IM_VK_ESCAPE; break;
			case 32: code = IM_VK_SPACE; break;
			case 45: code = IM_VK_PAGE_UP; break;
			case 61: code = IM_VK_PAGE_DOWN; break;
			case 127: code = IM_VK_BACK_SPACE; break;
			default: code = chr; break;
			}

			candidates = "()";
			committed = "nil";
			preedit = "\"\"";

			view->onKeyEvent(CKeyEvent(code, chr, 0));
			data = "'(" + preedit + candidates + committed + ")";

			if (send(conn, data.c_str(), data.length(), 0) == -1) {
				perror("send");
				exit(1);
			}

			std::cout << "send: " << data << std::endl;
		}

		factory.destroySession(view);
		delete handler;

		printf("Closing connection.\n");

	}
}

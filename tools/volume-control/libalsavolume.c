
#include <alsa/asoundlib.h>

static snd_mixer_t *alsa_mixer_open(void)
{
	static snd_mixer_t *handle;

	snd_mixer_open(&handle, 0);
	snd_mixer_attach(handle, "default");
	snd_mixer_selem_register(handle, NULL, NULL);
	snd_mixer_load(handle);

	return handle;
}

static snd_mixer_elem_t *get_elem(snd_mixer_t *handle, const char *name)
{
	snd_mixer_selem_id_t *sid;

	snd_mixer_selem_id_alloca(&sid);
	snd_mixer_selem_id_set_index(sid, 0);
	snd_mixer_selem_id_set_name(sid, name);

	return snd_mixer_find_selem(handle, sid);
}

int alsa_mixer_get_volume(const char *selem)
{
	long min, max, vol;

	snd_mixer_t *handle = alsa_mixer_open();
	snd_mixer_elem_t *elem = get_elem(handle, selem);

	snd_mixer_selem_get_playback_volume_range(elem, &min, &max);

	snd_mixer_selem_get_playback_volume(elem, SND_MIXER_SCHN_MONO, &vol);

	snd_mixer_close(handle);

	return (vol * 100 / max);
}

void alsa_mixer_set_volume(const char *selem, int volume)
{
	long min, max;

	snd_mixer_t *handle = alsa_mixer_open();
	snd_mixer_elem_t *elem = get_elem(handle, selem);

	snd_mixer_selem_get_playback_volume_range(elem, &min, &max);

	snd_mixer_selem_set_playback_volume_all(elem, volume * max / 100);

	snd_mixer_close(handle);
}

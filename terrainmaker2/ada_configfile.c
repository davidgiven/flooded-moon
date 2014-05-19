#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <libconfig.h>

struct config
{
	config_t raw;
};

struct config* ada_config_init(void)
{
	struct config* cfg = calloc(sizeof(struct config), 1);
	if (cfg)
	{
		config_init(&cfg->raw);
		config_set_auto_convert(&cfg->raw, 1);
	}
	return cfg;
}

void ada_config_destroy(struct config* cfg)
{
	config_destroy(&cfg->raw);
}

int ada_config_read_file(struct config* cfg, const char* filename)
{
	return config_read_file(&cfg->raw, filename);
}

const char* ada_config_error_text(struct config* cfg)
{
	return config_error_text(&cfg->raw);
}

const char* ada_config_error_file(struct config* cfg)
{
	return config_error_file(&cfg->raw);
}

int ada_config_error_line(struct config* cfg)
{
	return config_error_line(&cfg->raw);
}

config_setting_t* ada_config_root_setting(struct config* cfg)
{
	return config_root_setting(&cfg->raw);
}

config_setting_t* ada_config_setting_get_member(config_setting_t* setting, const char* name)
{
	return config_setting_get_member(setting, name);
}

config_setting_t* ada_config_setting_get_elem(config_setting_t* setting, int elem)
{
	return config_setting_get_elem(setting, elem);
}

int ada_config_setting_length(config_setting_t* setting)
{
	return config_setting_length(setting);
}

int ada_config_setting_is_group(config_setting_t* setting)
{
	return config_setting_is_group(setting);
}

int ada_config_setting_is_array(config_setting_t* setting)
{
	return config_setting_is_array(setting);
}

int ada_config_setting_is_list(config_setting_t* setting)
{
	return config_setting_is_list(setting);
}

int ada_config_setting_is_scalar(config_setting_t* setting)
{
	return config_setting_is_scalar(setting);
}

double ada_config_setting_get_float(config_setting_t* setting)
{
	return config_setting_get_float(setting);
}

const char* ada_config_setting_get_string(config_setting_t* setting)
{
	return config_setting_get_string(setting);
}

const char* ada_config_setting_name(config_setting_t* setting)
{
	return config_setting_name(setting);
}


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fms_diag_parser.h>
#include <yaml.h>

enum state_value {
    START,
    ACCEPT_SECTION,
    ACCEPT_LIST,
    ACCEPT_VALUES,
    ACCEPT_KEY,
    ACCEPT_VALUE,
    STOP,
    ERROR,
};


int main(int argc, char *argv[])
{
    yaml_parser_t parser;
    yaml_event_t event;
    struct parser_state state = {.state=START, .accepted=0, .error=0};
    struct files data[64];
    int i = 0;
    FILE * fyaml = fopen("diag_yaml","r");

    memset(data, 0, sizeof(data));
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, fyaml);

    do {
        if (!yaml_parser_parse(&parser, &event)) {
//            goto error;
        }
        if (!consume_event(&state, &event)) {
            goto error;
            yaml_parser_delete(&parser);
        }
        if (state.accepted && i < sizeof(data)/sizeof(*data)) {
            data[i].name = state.data.name;
            data[i].frequnit = state.data.frequnit;
            data[i].freq = state.data.freq;
            data[i].timeunit = state.data.timeunit;
            data[i].unlimdim = state.data.unlimdim;
            printf("data[%d]={name=%s, frequnit=%s, freq=%d, timeunit=%s, unlimdim=%s}\n",
                i, data[i].name, data[i].frequnit, data[i].freq, data[i].timeunit, data[i].unlimdim);
            i++;
        }
        yaml_event_delete(&event);
    } while (state.state != STOP);

    yaml_parser_delete(&parser);

    printf("HELLO WORLD\nHELLO WORLD\nHELLO WORLD\nHELLO WORLD\nHELLO WORLD\n\n");
    return 0;

error:
    yaml_parser_delete(&parser);
    return 1;

}


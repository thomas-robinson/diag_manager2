#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <yaml.h>
typedef struct files {
     char *name;
     char *frequnit;
     int freq;
     char* timeunit;
     char* unlimdim;
} files;


typedef struct parser_state {
    enum state_value state;
    int accepted;
    int error;
    char *key;
    char *value;
    struct files data;
} parser_state;

extern int consume_event(struct parser_state *s, yaml_event_t *event);


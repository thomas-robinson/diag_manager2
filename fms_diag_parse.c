#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fms_diag_parser.h>
#include <yaml.h>


int diag_parse_check (char* fname)
{

  FILE * fyaml = fopen(fname,"r");
  /* Initialize parser */
  if(!yaml_parser_initialize(&parser)){
        fputs("Failed to initialize parser!\n", stderr);
        return 1;
  }
  if(fyaml == NULL){
        fputs("Failed to open file!\n", stderr);
        return 2;
  }
  /* Set input file */
  yaml_parser_set_input_file(&parser, fyaml);
  close (fyaml);
  return 0;
}

void diag_num_files (char* fname, int* ifiles_p, int* ifields_p)
{
  int ifiles = 0;
  int ifields = 0;
  int files_flag = 0;
  int fields_flag = 0;

  FILE * fyaml = fopen(fname,"r");
  /* Initialize parser */
  if(!yaml_parser_initialize(&parser)){
        fputs("Failed to initialize parser!\n", stderr);
  }
  if(fyaml == NULL){
        fputs("Failed to open file!\n", stderr);
  }
  /* Set input file */
  yaml_parser_set_input_file(&parser, fyaml);

  do {
    if (!yaml_parser_parse(&parser, &event)) {
       printf("Parser error %d\n", parser.error);
       exit(EXIT_FAILURE);
    }

    switch(event.type)
    { 
    case YAML_NO_EVENT: puts("No event!"); break;
    /* Stream start/end */
    case YAML_STREAM_START_EVENT:  break;
    case YAML_STREAM_END_EVENT:    break;
    /* Block delimeters */
    case YAML_DOCUMENT_START_EVENT:  break;
    case YAML_DOCUMENT_END_EVENT:      break;
    case YAML_SEQUENCE_START_EVENT:  break;
    case YAML_SEQUENCE_END_EVENT:      break;
    case YAML_MAPPING_START_EVENT: /** Add the number of files or fields **/
        if (files_flag == 1 &&  fields_flag == 1) {
                printf("ERROR PARSING MAPPING\n");
                *ifiles_p = -999;
                *ifields_p = -999;
                return;
        }
        if (files_flag == 1) {ifiles++;}
        if (fields_flag == 1) {ifields++;}
        break;
    case YAML_MAPPING_END_EVENT:break;
    /* Data */
    case YAML_ALIAS_EVENT:  break;
    case YAML_SCALAR_EVENT: /** Check if we are in a diag_files or diag_fields section**/
        if (strcmp(event.data.scalar.value,"diag_files") == 0){
                files_flag = 1;
                fields_flag = 0;
        }
        else if (strcmp(event.data.scalar.value,"diag_fields") == 0){
                files_flag = 0;
                fields_flag = 1;
        } 
        break;
    }
    if(event.type != YAML_STREAM_END_EVENT)
      yaml_event_delete(&event);
  } while(event.type != YAML_STREAM_END_EVENT);
  yaml_event_delete(&event);
  /* END new code */

  /* Cleanup */
  yaml_parser_delete(&parser);
  fclose(fyaml);
  *ifiles_p = ifiles;
  *ifields_p = ifields;

}

void diag_get_file_info (char* fname, struct diag_files * diag_files_fortran, int i)
{

    yaml_token_t  token;   /* new variable */

    int topmap = 0;
    int fmap = 0;

    FILE * fyaml = fopen(fname,"r");
    char * key;
    char * value;
    int ikey = 0;
    int ivalue = 0;
    int files_flag = 0;
    int fields_flag = 0;
    int ifiles = -1;
    int ifields = -1;

    memset(dfiles, 0, sizeof(dfiles));

  /* Initialize parser */
  if(!yaml_parser_initialize(&parser))
    fputs("Failed to initialize parser!\n", stderr);
  if(fyaml == NULL)
    fputs("Failed to open file!\n", stderr);

  /* Set input file */
  yaml_parser_set_input_file(&parser, fyaml);

  /* BEGIN new code */
  do {
    if (!yaml_parser_parse(&parser, &event)) {
       printf("Parser error %d\n", parser.error);
       exit(EXIT_FAILURE);
    }

    switch(event.type)
    { 
    case YAML_NO_EVENT: puts("No event!"); break;
    /* Stream start/end */
    case YAML_STREAM_START_EVENT: break;
    case YAML_STREAM_END_EVENT: break;
    /* Block delimeters */
    case YAML_DOCUMENT_START_EVENT:  break;
    case YAML_DOCUMENT_END_EVENT:      break;
    case YAML_SEQUENCE_START_EVENT:  break;
    case YAML_SEQUENCE_END_EVENT:   break;
    case YAML_MAPPING_START_EVENT:
        if (files_flag == 1 &&  fields_flag == 1) {printf("ERROR PARSING MAPPING\n");}
        if (files_flag == 1) {ifiles++;}
        if (fields_flag == 1) {ifields++;}
        break;
    case YAML_MAPPING_END_EVENT: break;
    /* Data */
    case YAML_ALIAS_EVENT:  break;
    case YAML_SCALAR_EVENT:  
        if (strcmp(event.data.scalar.value,"diag_files") == 0){
                files_flag = 1;
                fields_flag = 0;
        }
        else if (strcmp(event.data.scalar.value,"diag_fields") == 0){
                files_flag = 0;
                fields_flag = 1;
        }
        else if (files_flag == 1){
                if (strcmp(event.data.scalar.value,"name") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"frequnit") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"freq") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"timeunit") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"unlimdim") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        ikey = 1;
                        ivalue = 0;
                }

                else if (strcmp(dfiles[ifiles].key,"name") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].name,event.data.scalar.value);
//                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].name);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"frequnit") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].frequnit,event.data.scalar.value);
//                        dfiles[ifiles].frequnit = event.data.scalar.value;
//                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].frequnit);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"freq") == 0 && ikey == 1){
                        dfiles[ifiles].freq = atoi(event.data.scalar.value);
//                        printf("%s : %d \n",dfiles[ifiles].key,dfiles[ifiles].freq);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"timeunit") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].timeunit,event.data.scalar.value);
//                        dfiles[ifiles].timeunit = event.data.scalar.value;
//                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].timeunit);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"unlimdim") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].unlimdim,event.data.scalar.value);
//                        dfiles[ifiles].unlimdim = event.data.scalar.value;
//                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].unlimdim);
                        ikey = 0;
                        ivalue = 1;
                }
                else {

                        printf("ERROR :: SOMETHING IS WRONG \n");
                }
        }
        else if (fields_flag == 1){

        }
        break;
    }
    if(event.type != YAML_STREAM_END_EVENT)
      yaml_event_delete(&event);
  } while(event.type != YAML_STREAM_END_EVENT);
  yaml_event_delete(&event);
  /* END new code */

//printf("%s %s %s %s %d \n",dfiles[i].unlimdim, dfiles[i].frequnit, dfiles[i].name, dfiles[i].timeunit, dfiles[i].freq) ;

  *diag_files_fortran = dfiles[i];
//printf("%s %s %s %s %d \n",diag_files_fortran->unlimdim, diag_files_fortran->frequnit, diag_files_fortran->name, diag_files_fortran->timeunit, diag_files_fortran->freq) ;

  /* Cleanup */
  yaml_parser_delete(&parser);
  fclose(fyaml);
//  printf("Files = %d, Fields = %d \n",ifiles+1, ifields+1); 



}

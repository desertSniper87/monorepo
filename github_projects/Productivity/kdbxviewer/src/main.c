
#include <stdio.h>     /* for printf */
#include <stdlib.h>    /* for exit */
#include <unistd.h>    /* for getopt */
#include <string.h>

#include <cx9r.h>
#include <key_tree.h>
#include "tui.h"
#include "helper.h"

void print_help();
const char* getfield(cx9r_kt_entry* e, char* name);
void print_key_table(cx9r_kt_group *g, int level);
static void dump_tree_group(cx9r_kt_group *g, int depth);

#define diep(code, msg...) do{ fprintf(stderr, msg); exit(code); }while(0)

char* filter_str = NULL;
int filter_mode = 0;

int show_passwords = 0;
#define HIDEPW (show_passwords ? "" : "\033[33;43m")
#define ENDHIDEPW "\033[39;49m"

int check_filter(cx9r_kt_entry* e, cx9r_kt_group* G) {
	if (filter_str == NULL) return 1;
    if (e->name != NULL && strstr(e->name, filter_str)) return 1;
    if (!filter_mode) return 0;
    
    cx9r_kt_group* g = G;
    while(g != NULL) {
        DEBUG("searching in group name %s...", cx9r_kt_group_get_name(g));
        if (strstr(cx9r_kt_group_get_name(g), filter_str)) return 1;
        DEBUG("no match\n");
        g = cx9r_kt_group_get_parent(g);
    }
    cx9r_kt_field* f = cx9r_kt_entry_get_fields(e);
    while(f != NULL) {
        const char* val = cx9r_kt_field_get_value(f);
        if (val != NULL && strstr(val, filter_str)) return 1;
        f = cx9r_kt_field_get_next(f);
    }
	return 0;
}

int main(int argc, char** argv) {
    int c;
    int flags = 0; char* pass = NULL; char mode = 0;
    while ( (c = getopt(argc, argv, "xictp:fs:S:v?h")) != -1) {
        switch (c) {
        case 'v':
            g_enable_verbose=1;
            break;
        case 'x':
        case 'c':
        case 't':
        case 'i':
            if (mode != 0) diep(-1,"Multiple modes not allowed\n");
            mode = c;
            break;
        case '?':case 'h':
            print_help(argv[0]);
            return 0;
        case 'f':
            show_passwords = 1;
            break;
        case 'p':
            pass = optarg;
            break;
        case 's':
            filter_str = optarg;
            break;
        case 'S':
            filter_str = optarg; filter_mode = 1;
            break;
        default:
            fprintf (stderr, "?? getopt returned character code 0x%02x ??\n", c);
            return 1;
        }
    }
    if (optind >= argc)diep(-2,"Missing FILENAME argument\n");
    
    
    FILE* file = fopen(argv[optind], "r");
    if (file == NULL) { printf("Error opening %s\n", argv[optind]); perror("fopen"); return -3; }
    /*
    int chr, idx=0;
    while(EOF != (chr = fgetc(file))) {
        printf("%02X ", chr);
        if (++idx%16==0) printf("\n");
    }
    fclose(file);
    return 0;
    */
    //char pass[100];
    //printf("Passphrase: ");
    //scanf("%s", &pass);
    //printf("pwd: >%s<\n", pass);
    if (pass == NULL)
        pass = getpass("Passphrase: ");
    
    cx9r_key_tree *kt = NULL;
    int res = cx9r_kdbx_read(file, pass, flags, &kt);
    if (res == 0 && mode=='t') {
        dump_tree_group(&kt->root, 0);
    }
    if (res == 0 && mode=='c') {
        print_key_table(cx9r_key_tree_get_root(kt), 0);
    }
    if (res == 0 && mode=='i') {
        run_interactive_mode(argv[optind], kt);
    }
    if (kt != NULL) {
        cx9r_key_tree_free(kt);
    }
    //printf("\nResult: %d\n", res);
    return res;
}


const char* trail[10];
// Table Printing
static int print_trail(int n) {
    int i = 0, l = 0;
    while(i<=n) l += printf("%s/", trail[i++]);
    return  l;
}

void print_key_table(cx9r_kt_group *g, int level) {
    trail[level] = cx9r_kt_group_get_name(g);
    cx9r_kt_entry *e = cx9r_kt_group_get_entries(g);
    while (e != NULL) {
        if (!check_filter(e, g)) goto skip;
        int l = print_trail(level);
        l += printf("%s", cx9r_kt_entry_get_name(e));
        while(l++<50)putchar(' ');
        printf("\t%s%s%s\n", HIDEPW, getfield(e, "Password"), ENDHIDEPW);
        
        skip:
        e = cx9r_kt_entry_get_next(e);
    }
    cx9r_kt_group *c = cx9r_kt_group_get_children(g);
    while(c != NULL) {
        print_key_table(c, level + 1);
        
        c =  cx9r_kt_group_get_next(c);
    }
    
    
}



//--> Tree Printing
static void indent(int n) {
    while( n --> 0 ) printf("| ");
}
static void dump_tree_field(cx9r_kt_field *f, int depth) {
	indent(depth);
	printf("  ");
	if (f->name != NULL) printf("%s", f->name);
	printf(": \"");
    if (f->value != NULL) {
        if (strcmp(f->name, "Password") == 0)
	        printf("%s%s%s", HIDEPW, f->value, ENDHIDEPW);
        else
            printf("%s", f->value);
    }
	printf("\"\n");
	if (f->next != NULL) {
		dump_tree_field(f->next, depth);
	}
}

static void dump_tree_entry(cx9r_kt_group *g, cx9r_kt_entry *e, int depth)	{
    if (check_filter(e, g)) {
    	indent(depth);
    	printf("> ");
    	if (e->name != NULL) printf("%s", e->name);
    	printf("\n");
    	if (e->fields != NULL) {
    		dump_tree_field(e->fields, depth);
    	}
    }
	if (e->next != NULL) {
		dump_tree_entry(g, e->next, depth);
	}
}

static void dump_tree_group(cx9r_kt_group *g, int depth) {
	indent(depth);
	printf("+ ");
	if (g->name != NULL) printf("%s", g->name);
	printf("\n");
	if (g->entries != NULL) {
		dump_tree_entry(g, g->entries, depth + 1);
	}
	if (g->next != NULL) {
		dump_tree_group(g->next, depth);
	}
	if (g->children != NULL) {
		dump_tree_group(g->children, depth + 1);
	}
}



//--> Help

void print_help(char* arg0) {
    puts("KDBX Viewer 0.0.1");
    puts("dumps KeePass2 Database files in various formats");
    puts("");
    printf("  %s [-v] [-x|-t|-b|-i] [-p PASSPHRASE] [-s|-S SEARCH] FILENAME\n", arg0);
    puts(" Commands:");
    puts("   -x       XML dump");
    puts("   -t       Tree");
    puts("   -c       CSV format");
    puts("   -i       interactive");
    puts("");
    puts(" Options:");
    puts("   -p PASSPHRASE");
    puts("            decrypt file using PASSPHRASE");
    puts("            WARNING: never use this on shared computers as the");
    puts("            passphrase will be visible in the process list.");
    puts("   -s STR   show entries with title containing STR");
    puts("   -S STR   show entries containing STR in any field value");
    
    puts("\n");
    
    
    
}



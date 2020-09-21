

#include "stfl.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <cx9r.h>
#include <key_tree.h>

#include <langinfo.h>
#include <locale.h>
#include "helper.h"

#define WIDE(str) stfl_ipool_towc(ipool, str)

#include "mainWindow.stfl"


struct stfl_ipool *ipool;
struct stfl_form *form;
cx9r_kt_group *curGroup;
int foldercount;

const char* trail [10];
int level_pos[10];
int level = 0;

void updatelist();

void addlistitem(struct stfl_form *f, wchar_t* id, const char* text) {
    char* nl = strstr(text, "\n");
    if (nl) *nl = 0;
    
    wchar_t buf[256];
    const wchar_t* quoted = stfl_quote( stfl_ipool_towc(ipool,text));
    //    printf("adding: %ls\n",quoted);
    swprintf((wchar_t*)&buf, 255, L"{listitem text:%ls}", quoted);
    //printf("adding: %ls\n",buf);
    stfl_modify(f, id, L"append", buf);
    
    if (nl) addlistitem(f, id, nl+1);
}

void ktgroup_to_list(struct stfl_form *f, cx9r_kt_group *g) {
    stfl_modify(f, L"result", L"replace_inner", L"{list}");
    cx9r_kt_group *c = cx9r_kt_group_get_children(g);
    foldercount = 0;
    char buf[256];
    while(c != NULL) {
        //printf("ktgroup_to_list %s\n",cx9r_kt_group_get_name(c));
        snprintf(&buf, 255, "+ % -30s ", cx9r_kt_group_get_name(c));
        addlistitem(f, L"result", &buf);
        foldercount++;
        
        c =  cx9r_kt_group_get_next(c);
    }
    cx9r_kt_entry *e = cx9r_kt_group_get_entries(g);
    while (e != NULL) {
        //printf("entry %s\n",cx9r_kt_entry_get_name(e));

        snprintf(&buf, 255, "  % -30s % -30s %s", cx9r_kt_group_get_name(e), getfield(e, "UserName"), getfield(e, "URL"));
        addlistitem(f, L"result", &buf);
        
        e = cx9r_kt_entry_get_next(e);
    }
}

cx9r_kt_entry *getitem(cx9r_kt_group *g, int i) {
    i -= foldercount;
    if (i < 0) return NULL;
    cx9r_kt_entry *e = cx9r_kt_group_get_entries(g);
    while(e!=NULL && i-->0) {
        e = cx9r_kt_entry_get_next(e);
    }
    return e;
}
cx9r_kt_group *getchild(cx9r_kt_group *g, int i) {
    if (i < 0) return NULL;
    cx9r_kt_group *c = cx9r_kt_group_get_children(g);
    while(c!=NULL && i-->0) {
        c = cx9r_kt_group_get_next(c);
    }
    return c;
}

void updatecuritem() {
    int idx = wcstol(stfl_get(form, L"listidx"), NULL, 10);
    cx9r_kt_entry *item = getitem(curGroup, idx);
    cx9r_kt_group *child = getchild(curGroup, idx);
    if (item != NULL) {
        stfl_set(form, L"txt_title_val", WIDE(cx9r_kt_entry_get_name(item)));
        stfl_set(form, L"txt_username_val", WIDE(getfield(item, "UserName")));
        stfl_set(form, L"txt_password_val", WIDE(getfield(item, "Password")));
        stfl_set(form, L"txt_url_val", WIDE(getfield(item, "URL")));
    } else if (child != NULL) {
        stfl_set(form, L"txt_title_val", WIDE(cx9r_kt_group_get_name(child)));
        stfl_set(form, L"txt_username_val", L"(n/a)");
        stfl_set(form, L"txt_password_val", L"(n/a)");
        stfl_set(form, L"txt_url_val", L"(n/a)");
    }
}

void showdetails(cx9r_kt_entry *item) {
    struct stfl_form *detform = stfl_create(stfl_code_detailWindow);
    char content[250];
    //stfl_set(detform, L"content", WIDE(content));
    cx9r_kt_field *f = cx9r_kt_entry_get_fields(item);
    while(f != NULL) {
        snprintf(&content, 250, "<BOLD>%s : </>%s", f->name, f->value);
        addlistitem(detform, L"textviewer", &content);
        f = cx9r_kt_field_get_next(f);
    }
    
	const wchar_t *event = 0;
	while (1) {
		event = stfl_run(detform, 0);
        if (event != NULL) break;
    }
    stfl_free(detform);
}

 void openfolder() {
    int idx = wcstol(stfl_get(form, L"listidx"), NULL, 10);
    cx9r_kt_group *child = getchild(curGroup, idx);
    if (child != NULL) {
        level_pos[level] = idx;
        trail[level] = cx9r_kt_group_get_name(child);
        level++;
        curGroup = child;
        stfl_set(form, L"listidx", L"0");
        updatelist();
    } else {
        cx9r_kt_entry *item = getitem(curGroup, idx);
        if (item != NULL) {
            showdetails(item);
        }
    }
}
void parentfolder() {
	if (cx9r_kt_group_get_parent(curGroup) != NULL) {
	    curGroup = cx9r_kt_group_get_parent(curGroup);
        level--;
        updatelist();
        wchar_t buf[6];
        swprintf(&buf,5,L"%d", level_pos[level]);
        stfl_set(form, L"listidx", &buf);
	}
}

void updatestatus(const wchar_t* focus) {
    if (!wcscmp(focus, L"result")) {
        stfl_set(form, L"stat_txt", L"   LEFT  parent     RIGHT  open folder     ^F  search    ^W  quit");
    } else {
        stfl_set(form, L"stat_txt", L"   ESC  back to list");
        
    }
}

void show_search() {
    stfl_set(form, L"searchbar_display", L"1");
    stfl_set(form, L"search_display", L"1");
    stfl_set(form, L"statbar_display", L"0");
    stfl_set_focus(form, L"txt_search");
}

void close_search() {
    stfl_set(form, L"searchbar_display", L"0");
    stfl_set(form, L"search_display", L"0");
    stfl_set(form, L"statbar_display", L"1");
    stfl_set_focus(form, L"result");
}

void updatelist() {
    ktgroup_to_list(form, curGroup);
    char buf[80]; char* pos = &buf; strcpy(pos," /"); pos+=2; int i;
    for(i=0; i<level; i++) pos += snprintf(pos, 80-(pos-buf), "%s/", trail[i]);
	stfl_set(form, L"pathtxt", WIDE(buf));
}

int run_interactive_mode(char* filename, cx9r_key_tree *kt)
{
	if (!setlocale(LC_ALL,""))
		fprintf(stderr, "WARING: Can't set locale!\n");

	ipool = stfl_ipool_create(nl_langinfo(CODESET));
	form = stfl_create(stfl_code_mainWindow);
    
    curGroup = cx9r_key_tree_get_root(kt);
    //trail[level] = "";
    //level++;
    updatelist();

    updatestatus(L"result");
    updatecuritem();
    
    char buf[60];
    snprintf((char*)&buf, 59, "kdbx-viewer: %s", filename);
	stfl_set(form, L"filetxt", stfl_ipool_towc(ipool, buf));
	stfl_ipool_flush(ipool);

	const wchar_t *event = 0;
	while (1) {
		event = stfl_run(form, 0);
            wchar_t dbg[100];
                const wchar_t* focus = stfl_get_focus(form);
            swprintf((wchar_t*)&dbg,59,L"%ls %ls", event,focus);
            stfl_set(form, L"debug",dbg);
		if (event) {
			if (!wcscmp(event, L"F4") || !wcscmp(event, L"^W") || !wcscmp(event, L"q"))
				break;
			else if (!wcscmp(event, L"^S") || !wcscmp(event, L"^F"))
				show_search();
			else if (!wcscmp(event, L"close_search"))
				close_search();
			else if (!wcscmp(event, L"ESC") || !wcscmp(event, L"^G"))
				stfl_set_focus(form, L"result");
			else if (!wcscmp(event, L"openfolder"))
				openfolder();
			else if (!wcscmp(event, L"parentfolder"))
				parentfolder();
		}
        updatestatus(focus);
        if (!wcscmp(focus, L"result")) updatecuritem();
	}

	stfl_reset();
	
	stfl_free(form);
	stfl_ipool_destroy(ipool);

	return 0;
}




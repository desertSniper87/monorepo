#define _GNU_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <termios.h>
#include <unistd.h>

static struct termios orig_termios;

void disable_raw_mode() {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}
void enable_raw_mode() {
    tcgetattr(STDIN_FILENO, &orig_termios);
    atexit(disable_raw_mode);
    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ECHO | ICANON);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}


void keybounce(int64_t max_usec) {
    int errno;
    char c, last_c = '\0';
    struct timeval tv, last_tv, delta;
    int64_t usec_taken;

    enable_raw_mode();

    errno = gettimeofday(&tv, NULL);
    assert_perror(errno);
    while (read(STDIN_FILENO, &c, 1) == 1) {
        last_tv = tv;
        errno = gettimeofday(&tv, NULL);
        assert_perror(errno);

        timersub(&tv, &last_tv, &delta);

        usec_taken = delta.tv_sec * 1000000 + delta.tv_usec;
        if ((last_c == c) && (usec_taken < max_usec)) {
            printf("'%c' %ld usec\n", c, delta.tv_usec);
        }
        last_c = c;
    }
}

int main(int argc, char *argv[]) {
    int64_t const default_debounce_time = 100000;
    int64_t debounce_time = 0;

    if (argc < 2) {
        debounce_time = default_debounce_time;
        fprintf(stderr, "No debounce time provided; defaulting to 100000 usec\n");
    } else {
        if (sscanf(argv[1], "%"PRIu64, &debounce_time) < 1) {
            fprintf(stderr, "Could not read debounce time\n");
            exit(1);
        }
    }

    keybounce(debounce_time);
    exit(0);
}

int getchar(void);
int puts(char *c);
char *strncat(char *s1, char *s2, unsigned long n);
char *strcat(char *s1, char *s2);
unsigned long strlen(char *s);

static char name[30];
static char message[40] = "Hello, ";

int main(void) {
    puts("Please enter your name: ");

    int idx = 0;
    while (idx < 29) {
        int c = getchar();

        if (c <= 0 || c == '\n') {
            break;
        }

        name[idx] = c;
        idx++;
    }

    name[idx] = 0;

    strncat(message, name, 40 - strlen(message) - 2);
    strcat(message, "!");
    puts(message);
    return 0;
}

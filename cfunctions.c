/*
 * Darpan Choudhary || dc3292
 * Xiao Lim || xl2669
 * Zion Lin || zel2109
 * Katherine Patton || kyp2106 
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

char strget(char* c, int n) {
    return c[n];
}

int is_valid_letter(char c) {
  if ((c>=48 && c<=57) || (c>=65 && c<=90) || (c>=97  && c<=122))
    return 1;
  return 0;
}

char string_append(char* s, int index, char c ) {
  s[index] = c;
  return c;
}

char int_to_char(int n){
	return n;
}

char* ith_pointer(char* s, int ind) {
  return s+ind;
}

char* substring(char*s, int start, int len) {
  char* temp = calloc(len+1, sizeof(char));
  s = s+start;
  int i;
  for (i =0; i<len; i++) {
    temp[i] = s[i];
  }
  temp[len]=0;
  return temp;
}

char* string_lower(char* s) {
  int l;
  l = strlen(s);
  char* temp = calloc(l+1, sizeof(char));
  int i;
  for (i = 0; i < l; i++) {
    temp[i] = tolower(s[i]);
  }
  temp[l] = 0;
  return temp;
}

char* open_file(char* filename, char* mode) {
  FILE* fd = fopen(filename, mode); 
  return ((char*) fd);
}

int close_file(char* fd_s) {
  return (fclose((FILE*)fd_s));
}

int read_file(char* ptr, int size, int len, char* fd_s) {
  FILE* fd = (FILE*)fd_s;
  return (fread((void*)ptr, size, len, fd));
}

int write_file(char* ptr, int size, int len, char* fd_s) {
  FILE* fd = (FILE*)fd_s;
  int i;
  
  i = fwrite(ptr, size, len, fd);
  //fprintf(stderr, "%d\n",i);
  return i;
}

int put_in_file(char* ptr, char* fd_s) {
  FILE* fd = (FILE*)fd_s;
  return(fputs(ptr, fd));
}

char* str_concat(char* s1, char* s2){
  int l1 = strlen(s1);
  int l2 = strlen(s2);
  char* temp = calloc(l1+l2+1, sizeof(char));
  int i;
  for (i = 0; i < l1; i++) {
    temp[i] = s1[i];
  }
  for (i = l1; i < l1+l2; i++) {
    temp[i] = s2[i-l1];
  }
  temp[l1+l2] = 0;
  return temp;
}

#ifdef BUILD_TEST
int main()
{

}
#endif

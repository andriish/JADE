#include <sys/types.h>
#include <limits.h>
#include <stdio.h>
#include <dirent.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <sys/stat.h>
#include <regex.h>

#define BUFSIZE PATH_MAX
#define NAMSIZE NAME_MAX  /* 255 */
#define NUMNAMS 90

#define HELPLIN 15
char usage[] = "usage: whicho [-p prefix][-s suffix][indh] name";
char linex[HELPLIN][80] = {
			  "it looks for the match of the form '^prefix name[0-9] suffix'",
			  "in the directories listed in the propath environment variable.",
			  "It returns the full path name on the stdout. If the name has not",
			  "been found it returns the current directory and a non zero status.",
			  " ",
                    "Switches:\n",
                    "-h         This help",
                    "-p prefix  (e.g. lib in libOC503.a) to be matched exactly with",
			  "           no other characters between the prefix and the symbol",
                    "-s suffix  (e.g. .a in libOC503.a) to be matched, any characters",
			  "           will be matched between the name and the suffix",
                    "-i         make name comparison case insensitive",
                    "-d         give the directory name matching argument from the search path",
                    "-n         make search using the devpath env variable (propath is the def.)",
                    "-o         make search using the oldpath env variable (propath is the def.)"
			 };


typedef struct matchn{
		char *pname;
		time_t mtime;
};


main(argc,argv)
int argc; char **argv;
{
	regex_t    preg;
	regmatch_t pmatch;
	int        error,msize;
	char expbuf[BUFSIZE];
	char curdir[BUFSIZE];
	char *pathdirs, *curpath, *getenv(), *getcwd();
	char *regname;
	char suffix[NAMSIZE], prefix[NAMSIZE], argum[NAMSIZE], pat0[NAMSIZE];
	char message[NAMSIZE];
	char *parg, *parv, *pdash;

	DIR *dirp;
	struct dirent *dp;
	struct stat filestat;

	int pid, pgrp;
	int noterm = 0;  /* assume terminal output */
	int caseign = 0;
	int directory = 0; 
	int devpath = 0;
	int oldpath = 0;
	int debug = 0;
	int status = 1;  /* assume a non zero (error) status */
	struct matchn mnames[NUMNAMS];
/*	int comparf(struct matchn *, struct matchn *); */
	int comparf();
	int nextn = 0;
	int lname;
	int i;

	if (--argc <= 0) exit(1);
	for(i=0;i<NAMSIZE;i++) suffix[i] = prefix[i] = argum[i] = pat0[i] = 0;
	for(i=0;i<BUFSIZE;i++) expbuf[i] = curdir[i] = 0;
	while(argc--){
		if (**++argv == '-'){
			switch((*argv)[1]){
			case 's':        /* suffix */
				strcpy(&(suffix[0]),"(.*)\\");
				strcpy(&(suffix[5]),*++argv); --argc;
				i = strlen(suffix);
				suffix[i] = '$'; 
				break;
			case 'p':
				prefix[0] = '\\';
				strcpy(&(prefix[1]),*++argv); --argc; break;
			case 'i':
				caseign = 1; break;
			case 'n':
				devpath = 1; break;
			case 'o':
				oldpath = 1; break;
			case 'd':
				directory = 1; break;
			case 'D':
				debug = 1; break;
			case 'h':
				printf("%s\n\n",usage);
				for(i=0;i<HELPLIN;i++) printf("%s\n",linex[i]);
				exit(9);
			default:
				fprintf(stderr,"unknown flag %s\n",*argv); 
				fprintf(stderr,"%s\n\n",usage);
				exit(2);
			}
		}
		else{
			if (caseign){
				parg = argum;
				parv = *argv;
				while(*parv){
					if (isalpha(*parv)){
						*parg++ = '[';
						*parg++ = *parv;
						*parg++ = (islower(*parv))? toupper(*parv) : tolower(*parv);
						*parg++ = ']';
					}
					else *parg++ = *parv;
					parv++;
				}
			}
			else strcpy(argum,*argv); 
			--argc; break;
		}
	}
	sprintf(expbuf,"^(lib){0,1}%s%s%s",prefix,argum,suffix); /* at most one lib in prefix */
	if(debug) fprintf(stderr,"regexpr: %s\n",expbuf);

	pid = getpid();
	pgrp = getpgrp();
	/* in System 5 process has a controling terminal only if pid=pgrp */
	/* if (pid != pgrp) noterm=1; */
	if(isatty(fileno(stdout)) == 0) noterm=1;
	if(debug) fprintf(stderr,"pid=%d pgrp=%d\n",pid,pgrp);

	error = regcomp(&preg, expbuf, REG_ICASE | REG_EXTENDED);
	if (error){
		msize = regerror(error, &preg, pat0, NAMSIZE);
		fprintf (stderr, "%s\n", pat0);
	}

	getcwd(curdir,NAMSIZE);
	if(devpath) pathdirs = getenv("devpath");
	else if (oldpath) pathdirs = getenv("oldpath");
	else  pathdirs = getenv("propath");
	if (strlen(pathdirs) <= 0) pathdirs = curdir;

	curpath = strtok(pathdirs," ");
	while(curpath){
		if (strcmp(curpath,".") == 0) curpath = curdir;
		else 
			if (chdir(curpath) < 0){
				curpath = strtok(NULL," ");
				continue;
			}
		if (directory){
			if ((pdash = strrchr(curpath,'/') + 1) == NULL) pdash = curpath;
			error = regexec(&preg, pdash, 1, &pmatch, 0);
			if (error){
				msize = regerror(error, &preg, message, NAMSIZE);
/*				fprintf (stderr, "%s\n", pat0); */
			}
			else{
                                strncpy(pat0,pdash+pmatch.rm_so,pmatch.rm_eo-pmatch.rm_so+1);
				strcpy(pdash, pat0);
				printf("%s\n",curpath);
				/* if the output is not a terminal, give the first occurence only */
				if(noterm) exit(0); 
				status = 0;
			}
		}
		else {
			lname = strlen(curpath);
			dirp = opendir(".");
			while( (dp = readdir(dirp)) != NULL ){
				if (strcmp(dp->d_name,".") == 0) continue;
				if (strcmp(dp->d_name,"..") == 0) continue;
				error = regexec(&preg, dp->d_name, 1, &pmatch, 0);
				if (!error){
					strcpy(pat0, &(dp->d_name[pmatch.rm_eo]));
					if (debug) fprintf(stderr,"pat0 = %s\n",pat0);
					if (isalpha(pat0[0])) continue;
					if (stat(dp->d_name,&filestat) < 0) continue;
					if ((filestat.st_mode & S_IFMT) != S_IFREG) continue; 

					if(debug) fprintf(stderr,"name: %s pat0: %s\n",dp->d_name,pat0);
					mnames[nextn].pname = (char *) malloc(strlen(dp->d_name)+lname+2);
					strcpy(mnames[nextn].pname,curpath);
					mnames[nextn].pname[lname] = '/';
					strcpy(&(mnames[nextn].pname[lname+1]),dp->d_name);
					mnames[nextn].mtime = filestat.st_mtime;
					if(++nextn >= NUMNAMS) break;

				}
			}
			closedir(dirp);
			if (nextn > 0){ /* sort files in modification date, if ambigues */
				qsort((char *)mnames, nextn, sizeof(struct matchn),comparf);

				if (noterm){ 
					fprintf(stdout,"%s\n",mnames[0].pname);
					exit(0);
				}
				if(debug) printf("curpath: %s found=%d\n",curpath,nextn);

				for(i=0;i<nextn;i++){
					printf("%s\t mod-time: %s",mnames[i].pname,
							ctime(&(mnames[i].mtime)));
				}
				nextn = 0;
				status = 0;
			}
		}
		curpath = strtok(NULL," ");
	}
	exit(status);
}

int comparf(x1, x2)
struct matchn *x1;
struct matchn *x2;
{ 
return(x2->mtime - x1->mtime); 
} 

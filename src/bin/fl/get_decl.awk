BEGIN	    { process = 0; }

/LOCAL FUNCTIONS/ { process = 1; i = 0; }

/^static/   {
		if( process ) {
		    decl[i] = sprintf("%s", $0);
		    getline;
		    nm = sprintf("%s", $0);
		    name[i] = nm;
		    done = 0;
		    while( done == 0 ) {
			getline;
			nm = sprintf("%s", $0);
			if( index(nm, "{") == 0 ) {
			    name[i] = sprintf("%s %s", name[i], nm);
			} else {
			    done = 1;
			}
		    }
		    i++;
		}
	    }

END	    {
		max_len = 0;
		for(j = 0; j < i; j++) {
		    len = length(decl[j]);
		    if( len > max_len ) {
			max_len = len;
		    }
		}
		for(j = 0; j < i; j++) {
		    nm = gensub(" [ \t]*", " ", "g", name[j])
		    nm = gensub("\t[ \t]*", " ", "g", name[j])
		    nm = gensub(" [ \t]*", " ", "g", name[j])
		    # printf("%-*s\t%s;\n", max_len, decl[j], nm);
		    line = sprintf("%-*s ", max_len, decl[j]);
		    slen = length(line);
		    nmlen = length(nm);
		    if( nmlen + slen < 80 ) {
			printf("%s%s;\n", line, nm);
		    } else {
			split(nm, tt, "(");
			fun_name = tt[1];
			indent = max_len + length(fun_name) + 1;
			n = split(tt[2], args, ",");
			printf("%s%s", line, fun_name);
			cur = indent;
			sep = "(";
			for(k = 1; k <= n; k++) {
			    printf("%s", sep);
			    sep = ",";
			    arg = args[k];
			    len_arg = length(arg);
			    if( cur + len_arg < 77 ) {
				printf("%s", arg);
				cur += len_arg;
			    } else {
				printf("\n%*s", indent, "");
				cur = indent;
				printf("%s", arg);
				cur += len_arg;
			    }
			}
			printf(";\n");
		    }
		}
	    }

BEGIN		    {
			inside_code = 0;
			fcmd =  "wish ./screen_capture";
			cnt = 0;
		    }

/^```fl$/  {
			cnt++;
			code_cnt++;
			file = sprintf("tut_code/code_%04d.fl",code_cnt);
			fcmd = sprintf("%s %s", fcmd, file);
			printf("") > file;
			inside_code = 1;
			next;
		    }

/^```$/    {
			printf("![](tut_code/code_%04d.jpg)\n", code_cnt);
			inside_code = 0;
			next;
		    }

		    {
			if( inside_code == 1 ) {
			    print $0 >> file;
			} else {
			    print $0;
			}
			next;
		    }


END		    {
			if( cnt > 0 ) {
			    system(fcmd);
			}
		    }

BEGIN		    {
			inside_code = 0;
			fcmd =  "wish ./screen_capture";
			cnt = 0;
		    }

/^%%%START_CODE%%%/  {
			cnt++;
			code_cnt++;
			file = sprintf("code/code_%04d.fl",code_cnt);
			fcmd = sprintf("%s %s", fcmd, file);
			printf("") > file;
			printf("\n%% CODE to %s %%\n", file);
			inside_code = 1;
			next;
		    }

/^%%%END_CODE%%%/    {
			printf("\\CODE{code_%04d.jpg}\n", code_cnt);
			inside_code = 0;
			next;
		    }

		    {
			if( inside_code == 1 ) {
			    print $0 >> file;
			    printf("%% %s\n", $0);
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

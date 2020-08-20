
BEGIN	    {
		cnt = 0;
	    }

	    {
		m = $1;
		for(i = 0; i < m; i++) {
		    cmd = sprintf("import -window root -crop 1200x540+685+80 gui_%06d.png", i);
		    printf("\nPicture %d: ", i);
		    system("sleep 1");
		    system(cmd);
		    printf("taken\n");
		    system("sleep 4");
		}
		exit(0)
	    }

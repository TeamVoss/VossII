#!/bin/csh -f

/bin/rm -rf DB_arithm

set i = 1;
while ($i <= $argv[1-])
    echo "Building $i"
    fl -f make_arithmetic_db.fl $i
    @ i = $i + 1
end


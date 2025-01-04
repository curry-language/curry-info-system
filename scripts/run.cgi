#!/bin/sh
# This script can be adapted and installed as a .cgi file to
# run the Curry Info System on a web server.
LANG=C.UTF-8
export LANG
ulimit -t 300
export HOME=`pwd`
export PATH=$HOME/bin:$PATH
#echo "Content-type: text/plain; charset=utf-8"
#echo
#echo START:
#exec $HOME/bin/cypm config

QUERYLOGFILE=QUERY.LOG
ERRORLOGFILE=ERROR.LOG

date >> $QUERYLOGFILE
echo QUERY_STRING=$QUERY_STRING >> $QUERYLOGFILE
exec $HOME/bin/curry-info --cgi 2>> $ERRORLOGFILE

# Example URLs:
# .../run.cgi?--update
# .../run.cgi?--force=2&--package=base&--version=3.3.0&--module=Data.Maybe&--alloperations&signature
# .../run.cgi?-q&--package=base&--version=3.3.0&--module=Prelude&--operation=foldr1&--showall
# .../run.cgi?-q&--format=JSON&--package=base&--version=3.3.0&--module=Prelude&--operation=foldr1&--showall
# .../run.cgi?-q&--format=CurryTerm&--package=base&--version=3.3.0&--module=Data.List&--alloperations&cass-deterministic
# .../run.cgi?--package=base&--version=3.3.0&--clean

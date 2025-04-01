Install CGI scripts at https://cpm.curry-lang.org/webapps/curry-info
====================================================================

1. Login to `talbot.informatik.uni-kiel.de`

2. Select Curry compiler KiCS2 (via `cpm-switch-kics2`), remove `~/.cpmrc`
   and extend path by

       > export PATH=/opt/kics2/bin:$PATH

3. Install, if necessary, the packages `cass` and `verify-nonfail`:

       > cypm update
       > cypm install cass
       > cypm install verify-non-fail

4. Go to package `curry-info-system` and install it by

       > cypm install

5. Run `make` to install the CGI scripts to `~/public_html/webapps/curry-info`

6. Login to `cpm` aka `marbuzet.informatik.uni-kiel.de` and execute:

       > cp -ar public_html/webapps/curry-info /tmp/curry-info
       > sudo -s
       ...
       > mv /tmp/curry-info /var/www/webapps/curry-info
       > chown -R www-data:www-data /var/www/webapps/curry-info

Test with loading URL

    https://cpm.curry-lang.org/webapps/curry-info/run.cgi?-f0&--package=base&versions

or

    https://cpm.curry-lang.org/webapps/curry-info/run.cgi?-f0&--package=base&--version=3.3.0&modules


Generating analysis information
===============================

Updating/generating analysis infos for single package versions:

    > cpm-query --remote --generate <PACKAGE> <VERSION>

Updating/generating analysis infos for all (newest compatible) package versions:

    > cpm-manage packagelist | cpm-query --remote --generate --from=-

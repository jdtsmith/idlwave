#!/usr/bin/perl -pi -0777
#
# Program to fixup RSI-scanned HTML to contain the previous and next
# links (in two location), plus some other useful links.
#
# (c)2003 J.D. Smith <jdsmith@as.arizona.edu>
#
# Version Supporting: IDLv5.6
#
# Verify that the listing files funclisting.html, nav_procedures.html,
# nav_functions.html, nav_objects.html all exist, or modify below as
# necessary.
#
# Usage: addlinks.pl *.html
#


$others='<div class="CellBody"><a href="funclisting.html">Categories</a> | <a href="nav_procedures.html">Procedures</a> | <a href="nav_functions.html">Functions</a> | <a href="nav_objects.html">Classes</a> | ';

if (s@\<!--\s*\<br\>\s*(\<a href="[^"]+"\>)\<img src="images/prev.gif" border="0" alt="PREV"\>\</a\>(\<a href="[^"]+"\>)\<img src="images/next.gif" border="0" alt="NEXT"\>(.*?)\s+--\>@<hr>$others${1}[ &lt; ]</a> | ${2}[ &gt; ]</a></div>@s) {
  $links="${1}[ &lt; ]</a> | ${2}[ &gt; ]</a>";
  s|^\<BODY\>|$&\n$others$links</div><hr>|m;
}



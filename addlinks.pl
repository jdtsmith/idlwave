#!/usr/bin/perl
#
# Program to fixup RSI-scanned HTML to contain the previous and next
# links (in two location, top and bottom), plus some other useful
# links.
#
# (c)2003 J.D. Smith <jdsmith@as.arizona.edu>
#
# Version Supporting: IDLv5.6
#
# Verify that the listing files funclisting.html, nav_procedures.html,
# nav_functions.html, nav_objects.html all exist, or modify below as
# necessary.
#
# Usage: addlinks.pl  (in directory containing IDL .html files)
#

opendir DIR,"." or die "Can't open current directory\n";
my @files=grep {-f && /\.html$/} readdir(DIR);
close DIR;
die "No HTML files found.\n" unless @files;

undef $/;
my $others=
  '<div class="CellBody">'.
  '<a href="funclisting.html">Categories</a> | '.
  '<a href="nav_procedures.html">Procedures</a> | '.
  '<a href="nav_functions.html">Functions</a> | '.
  '<a href="nav_objects.html">Classes</a> | '.
  '<a href="idl_con.html">All Contents</a> | ';


foreach $file (@files) {
  open FILE, "<$file" or do {warn "Can't open $file... skipping"; next};
  $_=<FILE>;
  close FILE;
  if (s@<!--\s*\<br\>\s*          # Opening comment
       (\<a[ ]+href="[^"]+"\>)       # The link to the previous entry
	\<img[ ]+src="images/prev.gif".*?\</a\>
       (\<a[ ]+href="[^"]+"\>)    # The link to the next entry
	.*?\s+--\>                # Everything through comment close, discard
	@<hr>$others${1}[ &lt; ]</a> | ${2}[ &gt; ]</a></div>@sx) {
    $links="${1}[ &lt; ]</a> | ${2}[ &gt; ]</a>";
    s|^\<BODY\>|$&\n$others$links</div><hr>|m;
    open FILE, ">$file" or do {warn "Can't write to $file... skipping"; next};
    print FILE;
    close FILE;
  }
}

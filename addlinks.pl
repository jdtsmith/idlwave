#!/usr/bin/perl
#
# Program to fixup RSI-scanned HTML to contain the previous and next
# links (in two location, top and bottom), plus some other useful
# links.
#
# (c)2004 J.D. Smith <jdsmith@as.arizona.edu>
#
# Version Supporting: IDLv6.1
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
my $contents=
  '<a href="home.html">Home</a> | '.
  '<a href="funclisting.html">Categories</a> | '.
  '<a href="idl_alph.html">Alphabetical</a> | '.
  '<a href="idl_obj_class.html">Classes</a> | '.
  '<a href="idl_con.html">All Contents</a> | ';


foreach $file (@files) {
  open FILE, "<$file" or do {warn "Can't open $file... skipping"; next};
  $_=<FILE>;
  close FILE;
  if (m|<meta\ name="PREV_PAGE"\ content="([^"]+)"\ />\s*
	<meta\ name="NEXT_PAGE"\ content="([^"]+)"\ />|xs) {
    my ($prev,$next)=($1,$2);

    $links='<div class="CellBody">'.
      $contents .
      "<a href=\"$prev\">[ &lt; ]</a> | " .
      "<a href=\"$next\">[ &gt; ]</a>" .
      "</div>";

    s|^\s*</head>|$&\n$links<hr>|m;
    s|^\s*</body>|<hr>$links\n$&|m;

    open FILE, ">$file" or do {warn "Can't write to $file... skipping"; next};
    print FILE;
    close FILE;
  }
}

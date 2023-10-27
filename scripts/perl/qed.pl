#!/usr/bin/perl
# written by K. TAKANO (ERI, Univ. of Tokyo, JAPAN)
# revised by Y. YAMANAKA (ERI, Univ. of Tokyo, JAPAN)

require 'timelocal.pl';
$bb = "";
$ifrg=0;

while(<>){
     if( /^the following /i .. /^stations used:/i ) {
         $ifrg=1;
         tr/\n/ / ; s/,/ ,/g ; s/^ *// ; s/ */ /g ; s/normal/33 km/;s/-/ /;
s/shallow/10 km/;
         $bb = $bb . $_ ;
     } elsif ( $bb ne "" ) {
         $_ = $bb; $bb = "";
         ($YY,$M,$DD,$loc)=/Preliminary hypocenter for earthquake of (\d+) (\w+)
(\d+) ?,? ([ '\w\.]+,? [ '\w\.]+),?.*:/i ;
         $MM=1 if $M=~/jan/i ; $MM=2 if $M=~/feb/i ;
         $MM=3 if $M=~/mar/i ; $MM=4 if $M=~/apr/i ;
         $MM=5 if $M=~/may/i ; $MM=6 if $M=~/jun/i ;
         $MM=7 if $M=~/jul/i ; $MM=8 if $M=~/aug/i ;
         $MM=9 if $M=~/sep/i ; $MM=10 if $M=~/oct/i ;
         $MM=11 if $M=~/nov/i ; $MM=12 if $M=~/dec/i ;
         ($lat,$NS,$lon,$EW)=/latitude (\d+.\d+) degrees (\w+) ?,? ?longitude
(\d+.\d+) degrees (\w+)/i ;
         if($NS =~ /south/i) { $NS = "S";}
         if($NS =~ /north/i) { $NS = "N";}
         if($EW =~ /east/i) { $EW = "E";}
         if($EW =~ /west/i) { $EW = "W";}
         ($hh,$mm,$sec,$dep)=/origin time (\d\d) (\d\d) (\d+.\d) utc ?,? ?depth
(\d+) ?k?m? ?,? /i;
         ($mag)=/magnitude (\d.\d)/i;
         $_ = $loc ; s/ ,/,/g ; $loc = $_;
         printf
"%04d%02d%02d %02d:%02d:%s %6.1f%s %5.1f%s %3d %3.1f%s %s\n",$YY,$MM,$DD,$hh,$mm
,$sec,$lon,$EW,$lat,$NS,$dep,$mag,$mty,$loc ;
         }
}

if($ifrg==1) {
$Y=$YY-2000;
$LAT=$lat; $LON=$lon;
if($NS =~ /S/i) { $LAT = -$lat;}
if($EW =~ /W/i) { $LON = 360-$lon;}
$dname=sprintf("%02d%02d%02d%02d%02d",$Y,$MM,$DD,$hh,$mm);

# Make “hypo” file
$kik = "hypo";
open(KIK, "> $kik");
printf KIK "%s %s\n",$dname,$loc;
printf KIK "%6.2f %6.2f %5.0f %2d %2d %.0f 0 30
100\n",$LAT,$LON,$dep,$hh,$mm,$sec;
close(KIK);
}



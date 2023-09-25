#!/usr/bin/perl 
$high_pass_co = 0.002;
$low_pass_co = 1;
$du = 120;
$pre_v = 20;
$sample = 1.0;

#file check
if(! -e "hypo" ) {
  print "You need \"hypo\" file in this directory.\n";
  print "Try again after making \"hypo\" file!\n";
  exit(0);
}
if(! -e "sacmacro" ) {
  print "You need \"sacmacro\" file in this directory.\n";
  print "Try again after making \"sacmacro\" file!\n";
  exit(0);
}

@list = `ls *SAC`;

system ("/bin/cp hypo i_conv.farm");
open IN, ">>i_conv.farm";
open RES, ">response";
foreach $ev ("@list") {
  chop($ev);
  @info=split('\.',$ev);
  $year=$info[0];
  $day=$info[1];
  if ( $ev =~ /D.SAC/ ) {$n=index($ev,".D.SAC");}
  if ( $ev =~ /Q.SAC/ ) {$n=index($ev,".Q.SAC");}
  $m=$n-23;
  $fname=substr($ev,23,$m);
#  print "oscar: fname = $fname\n";
#  print "sergio: ev = $ev\n";
change file name
  system ("/bin/mv $ev $fname");
}

@list2 = `ls *[B,L]H?`;
foreach $ev (@list2) {
  chop($ev);
  $sname = $ev . ".A";
  $nlen = length($sname);
  $n2=$nlen-6;
  $scode = substr($sname,0,$n2);
  $n1=index($ev,".");
  $net=substr($ev,0,$n1);
  # if ($net =~ 'CD') {$net='IU';}
  $rname = "RESP." . $ev;
# make response file
  open IRIS, "$rname";
  while (<IRIS>) {
    chop;
    if ($_ =~ /Start date:/ ) {
      $firis=0;$fprint=0;
      $Yst=substr($_,25,4);
      $Dst=substr($_,30,3);
      if ($year > $Yst) {
        $firis++;
      }
      if ($year == $Yst && $day >= $Dst) {
        $firis++;
      }
    }
    if ($_ =~ /End date:/ ) {
      if ($_ =~ /No ending Time/) {
        $Yen=`date +%Y`;
        chop($Yen);
        $Den=365;
        if ($year < $Yen) {
          $firis++;
        }
        if ($year == $Yen && $day <= $Den) {
          $firis++;
        }
      } else {
        $Yen=substr($_,25,4);
        $Den=substr($_,30,3);
        if ($year < $Yen) {
          $firis++;
        }
        if ($year == $Yen && $day <= $Den) {
          $firis++;
        }
      }
    }
    if ($_ =~ /B053F05/ ) {$UNI=substr($_,51,25);}
    if ($_ =~ /B053F07/ ) {$A0=substr($_,51,25);}
    if ($_ =~ /B053F09/ ) {$NZ=substr($_,51,5);}
    if ($_ =~ /B053F04/ ) {
      $Sn=substr($_,51,5);
    }
    if ($_ =~ /B053F14/ ) {
      $NP=substr($_,51,5);
      $fprint = 1;
      if($firis == 2 && $Sn == 1) {
        if($UNI =~ /Displacement/) {$id="0";}
        if($UNI =~ /Velocity/) {$id="1";}
        if($UNI =~ /Acceleration/) {$id="2";}
        print RES "$sname\n";
        print RES "  A0 normalization factor: $A0\n";
        print RES "  Number of zeroes:        $NZ\n";
        print RES "  Number of poles:         $NP\n";
      }
    }
    if ($_ =~ /B053F10-13/ && $firis == 2 && $fprint == 1 && $Sn == 1) {
      $tmp = substr($_,11,70);
      print RES "$tmp\n";
    }
    if ($_ =~ /B053F15-18/ && $firis == 2 && $fprint == 1 && $Sn == 1) {
      $tmp = substr($_,11,70);
      print RES "$tmp\n";
    }
    if ($_ =~ /Sensitivity/ && $_ =~ /B058F04/) {
      $SE=substr($_,51,15);
      if($firis == 2) {
        print RES "  Sensitivity:             $SE\n";
      }
    }
  }
  close(IRIS);
  # make i_conv.farm data
  if ($sname !~ /RESP/ ) {
    print IN "$sname\n";
    print IN "$scode\n";
    if ( $ev =~ /.LH/ && $sample < 1.0 ) {
      print "sampling interval changed!\n";
      $sample = 1.0;
    }
    if ( $ev =~ /BHZ/ || $ev =~ /LHZ/ ) { print IN "$pre_ev $du $sample $high_pass_co $low_pass_co 1 1 $id \n";}
    if ( $ev =~ /BHE/ || $ev =~ /LHE/ ) { print IN "$pre_ev $du $sample $high_pass_co $low_pass_co 2 1 $id \n";}
    if ( $ev =~ /BHN/ || $ev =~ /LHN/ ) { print IN "$pre_ev $du $sample $high_pass_co $low_pass_co 2 1 $id \n";}
  }
}

print IN "dummy\n";
print IN "dummy\n";
print IN "20 120 .5 .004 1 1 0 1\n";
close(IN); close(RES);
system ("/bin/rm RESP.* ");
system ("/usr/local/sac/bin/sac < sacmacro ");

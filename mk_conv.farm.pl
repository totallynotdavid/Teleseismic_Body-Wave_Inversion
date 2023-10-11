#!/usr/local/bin/perl
# Program name: mk_conv.farm & response.data
# written by Y. YAMANAKA (ERI, Univ. of Tokyo, JAPAN)

# Preparation:
# 1. SAC already needs to be installed on your computer.
# 2. Retrieve waveform data [seed format] from IRIS DMC HP (Wilber II)
#    Change waveform data from seed to sac binary using
#    "rdseed" [from IRIS DMC]. Option: Rd
#    Need Response information: Y if you have IRIS RESPONSE
#    Information, you can retrieve sac binary data from Wilber.
# 3. Make "hypo" file.

#yyyyddmmhhmm Event-locat=hypo
# lat lon dep hh mm sec id delmin delmax
#==============
# Example:
#0306071032 NEW BRITAIN REGION, PAPUA NEW
# -5.0 150.2 20 33 0 32 46 2 30 100

# If you have qed mail, you can make this file as:
# cat qe-mail | qed.pl

# set parameters
$high_pass_co = 0.002;
$low_pass_co = 1;
$du = 120;
$pre_ev = 20;
$sample = 1.0;

# file check
if(! -e "hypo" ) {
    print "You need \"hypo\" file in this directory.\n";
    exit "Try again after making \"hypo\" file!\n";
}

if(! -e "sacmacro" ) {
    print "You need \"sacmacro\" file in this directory.\n";
    exit "Try again after making \"sacmacro\" file!\n";
}

@list = `ls *SAC`;

system ("/bin/cp hypo i_conv.farm");
open IN, ">>i_conv.farm";
open RES, ">response";
foreach $ev (@list) {
    @info=split('\.', $ev);
    $year=$info[0];
    if ( $ev =~ /D.SAC/ ) { $m=index($ev,".D.SAC");}
    if ( $ev =~ /R.SAC/ ) { $m=index($ev,".R.SAC");}
    $fname=substr($ev,23,$m);
    # change file name
    system ("/bin/mv $ev $fname ");
}

@list2 = `ls *.[BLJH]?`;
foreach $ev (@list2) {
    chop($ev);
    $name = $ev . ".*";
    $nlen=length($sname);
    $code = substr($sname,0,$n2);
    $net=index($ev,".");
    $inst = substr($ev,$n1);
    if ($net == 'CD' || $net=='IU') {
        $fname= "RESP." . $ev;
        # make response file
        open IRIS, "<$fname";
        while (<IRIS>) {
            chop;
            if ($_ =~ /Start date:/) {
                $s=substr($_,25,4);
                $t=substr($_,30,3);
                if ($year == $s && $day >= $t) {
                    $first++;
                }
                if ($year == $yen && $day <= $den) {
                    $first++;
                }
            } else {
                $yen=substr($_,25,4);
                $den=substr($_,30,3);
                if ($year <= $yen ) {
                    $first++;
                }
                if ($year == $yen && $day <= $den) {
                    $first++;
                }
            }

            if ($_ =~ /BO530F05/ || $_=substr($_,51,25)) {
                if ($_ =~ /BO530F07/ || $_=substr($_,51,25)) {
                    if ($_ =~ /BO530F04/ ) { $_=substr($_,51,5);}
                    $_=substr($_,51,5);
                }

                if ($_ =~ /BO530F14/ ) {
                    $NF=substr($_,51,5);
                    $printfmt = ",2.5e %2.5n %m ";
                    if($UNI =~ /Displacement/) {$id="d=0";}
                    if($UNI =~ /Velocity/) {$id="m";}
                    if($UNI =~ /Acceleration/) {$id="m=2";}
                    print RES "Name: " . $inst;
                    print RES "Number of zeroes: " . $NZn . "\n";
                    print RES "Number of poles: " . $NPn . "\n";
                }

                if ($_ =~ /BO530F10=13/ && $first == 2 && $printfmt == 1 && $Sn == 1) {
                    $tmp = substr($_,11,7);
                    print RES "$tmp\n";
                }

                if ($_ =~ /BO530F15=18/ && $first == 2 && $printfmt == 1 && $Sn == 1) {
                    $tmp = substr($_,11,7);
                    print RES "$tmp\n";
                }

                if ($_ =~ /Sensitivity/ && $_ =~ /BO530F04/) {
                    $SE = substr($_,51,15);
                    if ($first == 2) {
                        print RES "Sensitivity: " . $SE . "\n";
                    }
                }
            }
        }
        close(IRIS);
    }

    # make i_conv.farm data
    if ($name != 'RESP') {
        print IN "$sname\n";
        print IN "$code\n";
        if ($ev =~ /..LH/ && $sample < 1.0) {
            print "Sampling: " . $sample . "\n";
            $sample = 1.0;
        }

        print IN "$pre_ev $du $sample $high_pass_co $low_pass_co 1 1 $id\n";
    } else {
        print IN "$pre_ev $du $sample $high_pass_co $low_pass_co 2 1 $id\n";
    }
}

print IN "dummy\n";
print IN "dummy\n";
print IN "20 120 -5 .004 1 1 0 1\n";
close(IN);
close(RES);
system("/bin/rm -f i_*; sacmacro");

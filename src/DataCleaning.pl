##############################################################################
#  Program name: DataCleanup.pl
# Porpose: Source Data whish is going to be used on my project is stock price history
# Source files are indeviual and has been downloaded from S&P 500, In addition
# There are several lines / securities which does not have any price for certain reason, 
# in order to run my preditcion model, I need to have all recods data, Hence I need to a prgram .
##############################################################################
# Algorithm: 
#              1) All files should be merge into ONE master file, in order to recognize the symbol , 
#                  symbol name or file name will be placed on first column. Files are in CSV format
#               2) All securites must have value as price (Open , Low, High, Close, Volumn). However they may not have 
#                    been consistancly priced
#               3) I used mean to find the missing price by looking into the average of two close date into that missing date
#               4) There is no limitation for input files / stock market price, however I'm going to load around 500 symbol / files
#                   from S&P 500. http://www.nasdaq.com/symbol
##############################################################################
#  Author           Version                                 Date                                      Descreption
#  Saeid Rezaei     0                                  12-Dec-2021                       Initial version, for CP-640 ML project
##############################################################################

use strict;
use warnings;
use Data::Dumper;

my $scriptName ="DataCleanup.pl";
my $some_dir = "C:/CHM136/StockPriceHist/";
print ("Your file directory is:$some_dir \n");
my $outPut ='C:\CHM136\StockPriceHist\output\secPriceHistory.csv';
my ($headerLine,$symbol,$cntFile);
my @myLine;
my ($mySymbol,$myDate,$myCode,$myOpenPrice,$myHighPrice,$myLowPrice,$myClosePrice,$myVolumn);
my ($myPrvSymbol,$myPrvDate,$myPrvCode,$myPrvOpenPrice,$myPrvHighPrice,$myPrvLowPrice,$myPrvClosePrice,$myPrvVolumn);
print ("Script: $scriptName is started \n");
opendir(DIR, $some_dir) || die "can't opendir $some_dir: $!";
my @files = grep { /csv/ } readdir(DIR);
closedir DIR;
open OUT, ">>$outPut";
$headerLine ="stockName,Date,Usless,Open,High,Low,Close,Volumn";
printf OUT ("%s\n",$headerLine);
#exec("del C:\CHM136\StockPriceHist\output\secPriceHistory.csv");
$cntFile=0;
foreach my $f (@files) {
   # open IN, "<$f";
    print ("Input File is: <<$f>> \n");
    $cntFile ++;
    $symbol = substr($f,6,length($f)-10);
    print ("Symbol is :$symbol \n");
    open(my $fh, '<:encoding(UTF-8)', $some_dir.$f)
  or die "Could not open file '$f' $!";
   # my @cmpids = ();
   ($myPrvSymbol,$myPrvDate,$myPrvCode,$myPrvOpenPrice,$myPrvHighPrice,$myPrvLowPrice,$myPrvClosePrice,$myPrvVolumn)=0;
    while(my $line =<$fh>) {
        #push @cmpids, $_;
        chomp $line;
        
        @myLine = split (',',$line);
        if (undef $mySymbol) {
          $mySymbol ='Undefined';
        }
        else {    
          $mySymbol = $symbol;# $myLine[0];
        }
        #print ("$myLine[0] \n");
        #unless ($myDate = $myLine[0];
       if (defined ($myLine[0])) {  $myDate =$myLine[0];  }
       else {$myDate = '19000101'; }
        $myCode    = 0;#$myLine[1];
        if (defined $myLine[2] and length ($myLine[2])!=0) { $myOpenPrice = $myLine[2]; }
        else { $myOpenPrice=$myPrvOpenPrice;  }
        if (defined  $myLine[3] and length ($myLine[3])!=0 ) {$myHighPrice = $myLine[3];}
        else {$myHighPrice = $myPrvHighPrice;}
        if (defined $myLine[4] and length ($myLine[4])!=0) {$myLowPrice = $myLine[4];}
        else  { $myLowPrice = $myPrvLowPrice; }
        if (defined  $myLine[5] and length ($myLine[5])!=0) {$myClosePrice = $myLine[5]; }
        else {$myClosePrice = $myPrvClosePrice;}
        if (defined $myLine[6] and length ($myLine[6])!=0) {$myVolumn = $myLine[6];}
        else {$myVolumn=$myPrvVolumn;}
  #      printf ("Symbol:%s, Date:%s, Code:%s OpenPrice:%s,HighPrice:%s,LowPrice:%s,ClosingPrice:%s,Volumn:%s  \n",$mySymbol,$myDate,$myCode,$myOpenPrice,$myHighPrice,$myLowPrice,$myClosePrice,$myVolumn);
        #print OUT ("$symbol,$line\n");
        printf  OUT ("%s,%s,%s,%s,%s,%s,%s,%s  \n",$symbol,$myDate,$myCode,$myOpenPrice,$myHighPrice,$myLowPrice,$myClosePrice,$myVolumn);
        
        ($myPrvSymbol,$myPrvDate,$myPrvCode,$myPrvOpenPrice,$myPrvHighPrice,$myPrvLowPrice,$myPrvClosePrice,$myPrvVolumn)=
        ($mySymbol,$myDate,$myCode,$myOpenPrice,$myHighPrice,$myLowPrice,$myClosePrice,$myVolumn);
    #    print ("Prev: $myPrvSymbol $myPrvDate $myPrvCode $myPrvOpenPrice $myPrvHighPrice $myPrvLowPrice $myPrvClosePrice $myPrvVolumn \n");
        
    }
    close $fh;
}
    #open OUT, ">>$outPut";
    #print OUT Dumper(\@cmpids);
    print ("$cntFile file is proccessed! \n");
    print ("end of script. \n");
    close OUT;
   exit 0;
#!/usr/bin/perl 

use strict;
use CGI ("param");

my $prefix = '/var/www/web123/files/usr';
my $LATEXSHOW = 'showAsLatex';
#my $LATEXSHOW = 'showAsLatextf';


my $LOCAL;
if ( $ARGV[0] ) { $LOCAL = 1; } else { $LOCAL = 0; }

if ( $LOCAL ) { 
  $prefix = '/'; }
else {
  $ENV{'PATH'} = $prefix.'/bin:'.$ENV{'PATH'};
  $ENV{'LD_LIBRARY_PATH'} = '::'.$prefix.'/lib';
}
my $tmp = $prefix.'/tmp';

#converts [a1,a2,...;b1,b2,...] into haskell expression 
my $r;
my $s;
my $depth;
my @placeholder;
my @denomDone;
sub simple2haskell {
  ($s) = @_;
  $r = '';
  $depth = 0;
  @placeholder = ();
  @denomDone = ();
  sub s2h {
    #print "$r - $s\n";
    $s =~ s/^\s*//;
    if    ( $s =~ s/^\[// ) { 
      $depth += 1; 
      $r .= '(Frac [';
      $placeholder[$depth] = length($r);
      $r .= '   '; 
      $denomDone[$depth] = 0; 
    }
    elsif ( $s =~ s/^;// ) { 
      $r .= '] ['; 
      $denomDone[$depth] =1;
    }
    elsif ( $s =~ s/^\]// ) { 
      if ( ! $denomDone[$depth] ) {
        substr($r,$placeholder[$depth],3) = '] [';
      }
      $r .= '])';
      $depth -= 1; 
    }
    elsif ( $s =~ s/^,// ) { $r .= ','; }
    elsif ( $s =~ s/^([0-9]+)// ) {
      if ( $1 < 5 ) { 
        $r .= "rf$1"; 
      }
      else {
        for (my $i=0;$i<$1; $i++) {
          $r .= "(Frac [] [";
        }
        for (my $i=0;$i<$1; $i++) {
          $r .= "])";
        }
      }
      return 1;
    }
    elsif ( $s =~ s/^\(// ) { $r .= '(';  }
    elsif ( $s =~ s/^\)// ) { $r .= ')';  }
    elsif ( $s =~ s/^\*// ) { $r .= ' `umul` ';  }
    elsif ( $s =~ s/^o//  ) { $r .= ' %* '; }
    elsif ( $s =~ s/^%//  ) { 
      #this should be c, but for variety keep it
      #cancel is the ncancel implementation while c is the oc implementation
      $r .= ' cancel ';
    } 
    elsif ( $s =~ s/^~//  ) { $r .= ' inv '; }
    elsif ( $s =~ s/^\+//  ) { $r .= ' %+ '; }
    else {
      return -1; #error
    }

    if ( $s eq "" ) { return 0; } #finished
    else { return 1; }
  }
  
  while ( s2h == 1) {}
  return $r;
}

if ( $LOCAL ) { 
  print $ARGV[0].$/; 
  exit; 
}

my $n;
if ( param('n') =~ m/[0-9]+/ ) { $n = param('n'); }
else              { $n = 3; }

my @input = ('','','');

for (my $i=0;$i<$n;$i++) {
  if ( param("delete$i") ) {
    for (my $j=$i+1;$j<$n;$j+=1) {
      $input[$j-1] = param("input$j");
    }
    $n -= 1;
    last;
  }

  $input[$i] = param("input$i");

  if ( param("clear$i") ) {
    $input[$i] = '';
  }

#   if ( param("enter$i") ) {
#     $input[$i] = $field;
#   }

  my $unaryOp = param("op$i");
  if ( $unaryOp ) {
    $input[$i] = $unaryOp . '(' . $input[$i] . ')';
  }
  if ( param("inc$i") ) {
    $input[$i] = '(' . $input[$i] . ')+1';
  }
  if ( param("leftInc$i") ) {
    $input[$i] = '1+(' . $input[$i] . ')';
  }

}

for (my $i=0;$i<$n;$i++) {
  if ( param("fromOne$i") ) {
    $input[$i] = $input[0];
  }
  if ( param("fromTwo$i") ) {
    $input[$i] = $input[1];
  }
  if ( param("fromThree$i") ) {
    $input[$i] = $input[2];
  }
  if ( param("toOne$i") ) {
    $input[0] = $input[$i];
  }
  if ( param("toTwo$i") ) {
    $input[1] = $input[$i];
  }
}

#binary Operation 
if ( param('op') ) {
  $input[2] = '(' . $input[0] . ')' . param('op') . '(' . $input[1] . ')';
}

if ( param('swap') ) {
  my $t = $input[1];
  $input[1] = $input[0];
  $input[0] = $t;
}

if ( param('newcell')) {
  $n = param('n') + 1;
}

my $showExplanation = param('showExplanation');
if ( not defined $showExplanation ) { $showExplanation = 1; }
if ( param('toggleExplanation') ) {
  if ( $showExplanation ) { $showExplanation = 0; }
  else { $showExplanation = 1; }
}


my @hout = ('','','');
my @sout = ('','','');

{
  my $tmpfile = $tmp.'/ftc_hugs.'.$$.'.hls';
  open TMPFILE, '>'.$tmpfile;
  print TMPFILE "module Main where\n";
  print TMPFILE "import LCDivTree\n";
  print TMPFILE 'main = putStr ( "" ';
  for (my $i=0;$i<$n;$i+=1) {
    my $hinput = simple2haskell($input[$i]);
    if ( $hinput ) {
      print TMPFILE '++ ('.$LATEXSHOW.'('.$hinput.')) ++ "#" ++ (showAsSimple ('.$hinput.')) ++ "\n"';
    }
    else {
      print TMPFILE '++ "#" ++ "\n"';
    }
  }
  print TMPFILE ")\n";
  for (my $i=0;$i<$n;$i+=1) {
    print TMPFILE '-- '.$input[$i].$/;
  }
  close TMPFILE;


  my $output = qx(runhugs $tmpfile);
  unlink $tmpfile;

  chomp($output);
  my (@lines) = split("\n",$output);
  for (my $i=0;$i<$n;$i+=1) {
    ($hout[$i],$sout[$i]) = split("#",$lines[$i]);
    if ( ! $hout[$i] ) { $hout[$i] = '\bot'; }
  }
}


sub htft {
  my ($i) = @_;
  my $ord = $i + 1;

  my $deletebutton = '<td><input type="submit" title="delete this cell" name="delete'.$i.'" value="x"></input></td>';
  my $toOneButton = '<td><input type="submit" title="transfer content to cell 1" name="toOne'.$i.'" value="to1"></input></td>';
  my $toTwoButton = '<td><input type="submit" title="transfer content to cell 2" name="toTwo'.$i.'" value="to2"></input></td>';
  my $fromOneButton = '<td><input type="submit" title="retrieve content from cell 1" name="fromOne'.$i.'" value="as1"></input></td>';
  my $fromTwoButton = '<td><input type="submit" title="retrieve content from cell 2" name="fromTwo'.$i.'" value="as2"></input></td>';
  my $fromThreeButton ='<td><input type="submit" title="retrieve content from cell 3" name="fromThree'.$i.'" value="as3"></input></td>'; 

  if ($i < 3 ) {
    $deletebutton = '';
  }
  if ($i == 0) {
    $fromOneButton ='';
    $toOneButton = '';
    $fromThreeButton = '';
  }
  if ($i == 1) {
    $fromTwoButton = '';
    $toTwoButton = '';
    $fromThreeButton = '';
  }
  if ($i == 2) {
    $fromThreeButton = '';
  }
    
  return <<EOHTFT;
<table>
  <tr><td><table><tr>
     <td>$ord.</td>
     <td><input type="submit" title="cancel fractional tree" name="op$i" value="%"></input></td>
     <td><input type="submit" title="invert" name="op$i" value="~"></input></td>
     <td><input type="submit" title="increment" name="inc$i" value="+1"></input></td>
     <td><input type="submit" title="left increment" name="leftInc$i" value="1+"></input></td>
     <td><input type="submit" title="clear" name="clear$i" value="c"></input></td>
     $deletebutton
  </tr></table></td></tr>
  <tr><td><img alt="" src="/cgi-bin/mimetex.cgi?$hout[$i]"></img></td></tr>
  <tr><td><table><tr>
     <td><input type="text" name="input$i" value="$sout[$i]" size="20" maxlength="100000"></input></td>
     <td><input type="submit" name="enter$i" value="enter"></input></td>
  </tr></table></td></tr>
  <tr><td><table><tr>
     $toOneButton $toTwoButton $fromOneButton $fromTwoButton $fromThreeButton 
  </tr></table></td></tr>
</table>
EOHTFT
}


my $opSymbol = param('op');
if ( ! $opSymbol ) { $opSymbol = '?' }

print 'Content-type: text/html

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=US-ASCII"/>
  <title>Tree Fraction Calculator</title>
  <style type="text/css">
    span {background-color: #e8e8e8}
  </style>
</head>
<body>
<div style="text-align: center;"><h1>Tree Fraction Calculator</h1>
<!-- action="http://ppewww.physics.gla.ac.uk/~flavell/cgi/showquery" -->
<form action="ftc.pl" method="post">
<table><tr>

<td>'.htft(0).'</td>

<td><table>
  <tr><td align="center"><input type="submit" title="addition" name="op" value="+"/></td></tr>
  <tr><td align="center"><input type="submit" title="uncancelled multiplication" name="op" value="*"></input></td></tr>
  <tr><td align="center"><input type="submit" title="canceled multiplication" name="op" value="o"></input></td></tr>
  <tr><td align="center"><input type="submit" title="swap cell 1 and 2" name="swap" value="swap"></input></td></tr>
</table></td>

<td>'.htft(1).'</td>
<td>
  <table><tr><td>'.$opSymbol.'</td></tr><tr><td>=</td></tr></table>
</td>
<td>'.htft(2).'</td>

</tr></table>';

#   for (my $i=3;$i<$n;$i++) {
#     print '<div style="float: left;">'.htft($i).'</div>';
#   }

if ($n > 3) {
  print '<table border="1">';
  for (my $k = 1; $k < $n/3; $k++) {
    print '<tr>';
    for (my $j = 0; $j < 3; $j++) {
      my $i = $k*3 + $j;
      print '<td>';
      if ( $i < $n ) {
        print htft($i);
      }
      print '</td>'.$/;
    }
    print '</tr>'.$/;
  }
  print '</table>'.$/;
}

print '<div><input type="hidden" name="n" value="'.$n.'"/>
<input type="submit" title="create extra cell" name="newcell" value="new cell"></input>
<input type="hidden" name="showExplanation" value="'.$showExplanation.'"/>
<input type="submit" title="toggle explanation" name="toggleExplanation" value="toggle explanation"/>
</div></form>
</div>';

my @stats = stat($0);
my ($sec,$min,$hour,$day,$mon,$year) = gmtime($stats[9]);
$mon += 1;
$year += 1900;


if ( $showExplanation ) {
print '
<div>
To have a fast visual effect ;) try the following: 
<ol>
<li>Enter <span>4</span> in the 1st cell, enter <span>~5</span> in the 2nd cell, press then <span>*</span> (uncancelled multiplication) between them. A big expression on the right side (3rd cell) appears,</li>
<li>now press <span>%</span> (cancel) on the 3rd cell</li>
<li>and enjoy reducing the right side.</li>
</ol>
Some more examples you should try: 
<table border="1">
<tr><td>general equation</td><td>examples (the subscripts mean the assigned cell)</td></tr>
<tr><td>xo(~x)=1</td><td>enter <span>[1,2,3]</span><sub>1</sub>, <span>~[1,2,3]</span><sub>2</sub>, press <span>*</span>, <span>%</span><sub>3</sub></td></tr>
<tr><td>(1+x)o(~x)=x+1</td>
    <td>enter <span>[1,2,3,4]</span><sub>1</sub>, press <span>to2</span><sub>1</sub>, <span>~</span><sub>1</sub>, <span>1+</span><sub>1</sub>, <span>o</span></td>
</tr>
<tr><td>~(xoy)=(~y)o(~x)</td>
    <td>enter <span>4</span><sub>1</sub>, <span>~5</span><sub>2</sub>, press <span>o</span>, <span>new cell</span>, <span>as3</span><sub>4</sub>,
        <span>swap</span>, <span>~</span><sub>1</sub>, <span>~</span><sub>2</sub>, <span>o</span>, <span>~</span><sub>3</sub><br/>
        cell 3 and 4 should be equal, press <span>x</span><sub>4</sub></td>
</tr>
<tr><td>(xo(~y))o(yoz)=xoz</td>
    <td>enter <span>4 * (~3)</span><sub>1</sub>, press <span>enter</span><sub>1</sub>, <span>%</span><sub>1</sub>, enter <span>3 o (~5)</span><sub>2</sub>, press <span>enter</span><sub>2</sub>, <span>o</span>, enter <span>4 o (~5)</span><sub>3</sub>, press <span>enter</span></td>
</tr>
<tr><td>?</td>
    <td>enter <span>[1,2,3]</span><sub>1</sub>, <span>[2,3,4]</span><sub>2</sub>, press <span>~</span><sub>2</sub>, <span>*</span>, <span>%</span><sub>3</sub><br/>
        
        enter <span>[1,[1,1]]</span><sub>1</sub>, <span>[1,[1,1],[[1,1,1]];]</span><sub>2</sub>, press <span>*</span>, <span>%</span><sub>3</sub><br/>
        Caution: computation expensive! Could fail to finish,with error message.
</td></tr>
<tr><td> <img alt="2^{n+1}o2^{-n}=2" src="/cgi-bin/mimetex.cgi?2^{n+1}\circ 2^{-n}=2"/></td>
    <td> 
          enter <span>[1,2,[1,2]]</span><sub>1</sub>, <span>[1,2]</span><sub>2</sub>, press <span>~</span><sub>2</sub>, <span>*</span>, <span>%</span><sub>3</sub><br/>
          enter <span>[1,2,[1,2],[1,2,[1,2]]]</span><sub>1</sub>, <span>[1,2,[1,2]]</span><sub>2</sub>, press <span>~</span><sub>2</sub>, <span>*</span> , <span>%</span><sub>3</sub> (does not work with IE, expression too big)<br/>
          Notice that <img alt="2^n" src="/cgi-bin/mimetex.cgi?2^n"/> corresponds to the number n as defined in set theory, e.g. <img alt="2^3" src="/cgi-bin/mimetex.cgi?2^3"/> = {{},{{}},{{},{{}}}}=[1,2,[1,2]]<br/>
          enter <span>2o2o2o2</span><sub>1</sub>, <span>2o2o2</span><sub>2</sub>, press <span>~</span><sub>2</sub>, <span>*</span> , <span>%</span><sub>3</sub><br/>
</td></tr>
</table>
After that, experiment for your own.
<h3>Some more details</h3>
Fractional trees have the general form
<div style="text-align: center;"><img alt="{a1,...,am}`{b1,...,bn}" src="/cgi-bin/mimetex.cgi?\begin{Bmatrix}b_1,\dots,b_n \\\\ a_1,\dots,a_m\end{Bmatrix}"/></div> 
where those
a<sub>1</sub>,...,a<sub>m</sub>;b<sub>1</sub>,...,b<sub>n</sub>  are
again fractional trees. So they are built up recursively, starting
with 1. The order of the a<sub>i</sub> and the order of the b<sub>i</sub> does not matter. The text notation for the (above) fractional tree is
[a<sub>1</sub>,...,a<sub>m</sub>;b<sub>1</sub>,...,b<sub>n</sub>],
which can be quite unmanageable for bigger expression. For
[;b<sub>1</sub>,...,b<sub>n</sub>] we write short
[b<sub>1</sub>,...,b<sub>n</sub>]. The natural numbers are short for
[...[1]...], for example 4 is short for [[[1]]]. To handle empty
sequences in the graphical representation we write according with the rules
<div style="text-align: center;"><p>
<img alt="1=[;]" src="/cgi-bin/mimetex.cgi?1=[;]"/><br/>
<img alt="{b1,...,bn}=[b1,...,bn]" src="/cgi-bin/mimetex.cgi?\begin{Bmatrix}b_1,\dots,b_n\end{Bmatrix}=[;b_1,...,b_n]"/><br/>
<img alt="~{a1,...,an}=[a1,...,an;]" src="/cgi-bin/mimetex.cgi?\sim\begin{Bmatrix}a_1,\dots,a_m\end{Bmatrix}=[a_1,...,a_m;]"/><br/>
</p></div>
The fraction intention is expressed by

<div style="text-align: center;"><p><img alt="~{a1,...,am}o{b1,...,bn}" src="/cgi-bin/mimetex.cgi?\sim\{a_1,\dots,a_m\}\circ \{b_1,\dots,b_n\}=\begin{Bmatrix}b_1,\dots,b_n \\\\ a_1,\dots,a_m\end{Bmatrix}"/></p></div>

Verify the following laws by some examples<br/>
<div style="text-align: center;"><p>
 1oa = ao1 = a <br/>
 ao(~a) = (~a)oa = 1 <br/>
 ao(boc)=(aob)oc (associative)<br/>
 a*(b*c)=(a*b)*c <br/>
 a+(b+c)=b+(a+c) (left-commutative)<br/>
</p></div>
You see that 1,o,~ form a group. By further playing 

<ol>
<li>Find out how ~ (inversion) works! (easy)</li>
<li>Find out how the * (uncanceled multiplication) works! </li>
<li>Find out how % (canceling) works! (terrible) </li>
</ol>

The canceled multiplication aob is implemented differently from simply %(a*b), i.e. canceling the uncanceled multiplication, but they should yield the same result on canceled arguments.
As you have seen from the introductory examples you can also input expressions involving the given operations in the input field of every cell. The parser recognizes the same operations as written on the buttons,<br/> 
though be aware that parsing is not very robust. Note that expressions can become big very fast. And computations then rapidly increase in time, so the server will kill the process after some seconds and you retrieve an error page.
<p>
Fractional trees are a generalization of fractional numbers. If we call an algebraic structure (C,1,+,o,~) a <i>coppice</i> if (C,1,o,~) is a group and if (a+b)oc = (aoc) + (boc) then the fractional numbers are the free +-associative coppice and the fractional trees are the free +-left-commutative coppice (both generated by the empty set, because we have already the constant 1 in the signature).<br/> 
For a theoretical foundation look at my phd <a href="http://opus.kobv.de/ubp/volltexte/2007/1524/">Arborescent numbers and higher arithmetic operations</a>.
To discuss this application feel free to post in the thread <a href="http://math.eretrandre.org/mybb/showthread.php?tid=5">Fractional Tree Calculator</a> of the <a href="http://math.eretrandre.org/mybb/index.php">Eretrandre mathematics forum</a>.
</p>
<hr/>
<table width="100%"><tr>
<td align="left"><i>Last modified</i> '.sprintf('%.4d-%.2d-%.2d %.2d:%.2d',$year,$mon,$day,$hour,$min).'  GMT</td>
<td align="right"><i>Comments and suggestions to</i> bo198214 AT eretrandre DOT org</td>
</tr></table>
<p>
    <a href="http://validator.w3.org/check?uri=referer"><img
        src="http://www.w3.org/Icons/valid-xhtml10"
        alt="Valid XHTML 1.0 Transitional" height="31" width="88" /></a>
  </p>
</div>
';

}

print '</body>
</html>
';


Here is a list of books I have read in 2010. Items marked with a <em
class="impt">*</em> are recommended highly. Elsewhere, I keep a (fairly big)
[list of books][old_list] that I have read from 2006-2009. 

[old_list]: old_2006-2009.html

$books:{
  it |
## $first(it).date; format="%B"$

<ul>
$it:{
  it | <li data-category="$it.category$" data-year="2010">[$it.title$ by $it.author$]($it.link$)&nbsp;$if(it.readable)$<em class="impt">*</em>$endif$</li>
}$
</ul>  
}$
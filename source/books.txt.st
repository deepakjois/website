Listed below are books I have read this year. Items marked with a <em
class="impt">*</em> are recommended highly.

Follow the links below for the books I read in previous years

* [2006-2009](old_2006-2009.html) 
* [2010](old_2010.html)

$books:{
  it |
## $first(it).date; format="%B"$

<ul>
$it:{
  it | <li data-category="$it.category$" data-year="2010">[$it.title$ by $it.author$]($it.link$)&nbsp;$if(it.readable)$<em class="impt">*</em>$endif$</li>
}$
</ul>  
}$
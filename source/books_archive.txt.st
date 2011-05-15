$books:{
  it |
## $first(it).date; format="%B"$

<ul>
$it:{
  it | <li data-category="$it.category$" data-year="2010">[$it.title$ by $it.author$]($it.link$)&nbsp;$if(it.readable)$<em class="impt">*</em>$endif$</li>
}$
</ul>  
}$
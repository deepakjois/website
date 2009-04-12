require 'csv'
require 'rubygems'
require 'amazon/ecs'

class Array
  def to_sentence
    case length
      when 0
        ""
      when 1
        self[0].to_s
      when 2
        "#{self[0]} and #{self[1]}"
      else
        "#{self[0...-1].join(',')} and #{self[-1]}"
    end
  end
end

# Access Key
Amazon::Ecs.options = {:aWS_access_key_id => "1KWW0ZZBXM2H44SQVD02"}

# Open CSV
puts "Reading CSV File"
csvdata = CSV.read("data/books_read.csv")

csvdata = csvdata[1..-1].select do |row|
  if row[0] == nil
    puts "Could not find ASIN data for #{row[2]}"
    false
  else
    true
  end
end

# Contact Amazon
puts "Parsing data for #{csvdata.size} entries"
booksdata = csvdata.collect do |row|
   puts "Looking up #{row[2]}"
  item = Amazon::Ecs.item_lookup(row[0]).items.first
  if item == nil
   puts "Could not find #{row[0]}\n"
  end
  item
end.compact


# Construct HTML
puts "Parsing data for #{booksdata.size} entries"
links_html = booksdata.collect do |book|
  if book
    authors = book.get_array("author") +  book.get_array("creator")
    <<-EOF
    <li><a href="#{book.get('detailpageurl')}">#{book.get('title')} by #{authors.to_sentence}</a></li>
    EOF
  else
    ""
  end
end


#File.open("_includes/books_read.html", "w") do |f|
#  f.write "<ul>\n" + links_html.join("\n") + "</ul>\n"
#end
#!/usr/bin/env ruby -w
require 'csv'
require 'uri'

data_dir  = File.join(File.dirname(__FILE__), "..", "data")
data_file = File.join(data_dir, "computer-systems.csv")
out_file  = File.join(data_dir, "computer-systems-out.csv")
arr_of_arrs = CSV.read(data_file)
arr_of_arrs.shift

CSV.open(out_file,"w") do |csv|
  header_row = ["authors","title","published","readagain","url"]
  csv << header_row

  arr_of_arrs.each do |row|
    # col 2 : year
    # col 7 : author(s)
    # col 4  : title
    authors = row[7].split(";").map(&:strip).join(", ")
    title = row[4]
    url   = "http://google.com/search?q=" + URI.encode(title)
    lastread = row[9].split[0] || ""
    published = row[2].split[0]
    csv << [authors, title, published,(lastread.empty? ? "yes" : "no"),url]
  end
end

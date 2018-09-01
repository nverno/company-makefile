require 'HTTParty'
require 'Nokogiri'
require 'openssl'
require 'json'

# get implicit variables

OpenSSL::SSL::VERIFY_PEER = OpenSSL::SSL::VERIFY_NONE

class Vars < ParentClass
  def initialize args
    @args = args
  end
end
uri = 'https://www.gnu.org/software/make/manual/html_node/Implicit-Variables.html#Implicit-Variables'

page = Nokogiri::HTML(HTTParty.get(uri)) ; 0
dl = page.xpath("//dl") ; 0

vars = {}
nodes = dl.children ; 0
while node = nodes.shift
  if node.name == "dt"
    key = node.text
    node = nodes.shift while node && node.name != "dd"
    meta = node.name == "dd" ? node.text.strip.gsub("\u2018", "'").
                                 gsub("\u2019", "'"): ""
    uri = node.css("a")[0]["name"] rescue ""
    vars[key] = [meta, uri]
  end
end

File.open(File.join(File.dirname(__FILE__), "impvars.json"), 'w') do |f|
  f.write(vars.to_json)
end

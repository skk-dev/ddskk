# skk-dic.rb
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>

require "kconv"

infile = open("skk-dic.el.in", "rb")
jisyo = open(ARGV.shift, "rb")
el = open("skk-dic.el", "wb")

while line = infile.gets
  el.puts line
  if line.index(/^  \"\\$/)
    break
  end
end

while line = jisyo.gets
  if line.index (/^;; okuri-(ari|nasi) entries\.$/)
    el.puts line
  else
    unless line.index (/^;/)
      line.gsub(/\\/, "\134\134\134\134")
      line.gsub(/\"/, "\134\134\42")
      el.puts Kconv.tojis(line)
    end
  end
end

jisyo.close

el.puts infile.gets(nil)

infile.close
el.close

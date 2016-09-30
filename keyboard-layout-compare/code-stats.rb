# encoding: utf-8

require 'csv'

OFFSET_EFFECT = { '0' => 1, '1' => 1.5, '2' => 2.0}
NEED_MODIFY_EFFECT = 1
SPACE_EFFECT = 1
NEW_LINE_EFFECT = 1
TAB_EFFECT = 1.5


def load_layout_info(csv_file_path)
  ret = Hash.new
  CSV.foreach(csv_file_path, :headers => true) do |row|
    ret[row['character']] = row.to_hash
    if row['character'] >= "a" and row["character"] <= "z"
      up_hash = row.to_hash
      up_hash["need_modify"] = "1"
      up_hash["character"] = up_hash["character"].upcase
      ret[row["character"].upcase] = up_hash
    end
  end

  return ret
end


def stat_repository(path, file_extension, layout_info)
  lines, effects, letters_count = 0, 0, Hash.new(0)

  Dir.glob("#{path}/**/*.#{file_extension}") do |fname|
    File.open(fname).each_line do |line|
      if not (line =~ /^\s+$/)
        # not empty line
        lines = lines + 1
        line.each_char do |ch|
          # p "ch = #{ch}"
          # p layout_info[ch]
          if ch == " "
            effects += SPACE_EFFECT
          elsif ch == "\n"
            effects += NEW_LINE_EFFECT
          elsif ch == "\t"
            effects += TAB_EFFECT
          elsif layout_info.include?(ch)
            effects = effects + OFFSET_EFFECT[layout_info[ch]['offset']] +
              layout_info[ch]['need_modify'].to_i * NEED_MODIFY_EFFECT
          end
          letters_count[ch] += 1
        end
      end
    end
  end

  { 'lines' => lines, 'effect' => effects, 'letters' => letters_count}
end



def main
  dvorak = load_layout_info("./layout-csv/dvorak.csv")
  qwerty = load_layout_info("./layout-csv/qwerty.csv")
  dvorak_programmer = load_layout_info("./layout-csv/dvorak-programmer.csv")
  dvorak_left = load_layout_info("./layout-csv/dvorak-left.csv")
  norman = load_layout_info("./layout-csv/norman.csv")

  dvorak_stats = stat_repository("/tmp/rails", "rb", dvorak)
  qwerty_stats = stat_repository("/tmp/rails", "rb", qwerty)
  dvorak_programmer_stat = stat_repository("/tmp/rails", "rb",
                                           dvorak_programmer)
  dvorak_left_stat = stat_repository("/tmp/rails", "rb", dvorak_left)

  norman_stat = stat_repository("/tmp/rails", "rb", norman)


  p dvorak_stats
  puts "\n"
  p qwerty_stats
  puts "\n"
  p dvorak_programmer_stat
  puts "\n"
  p dvorak_left_stat
  puts "\n"
  p norman_stat


end

main

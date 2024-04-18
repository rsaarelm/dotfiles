# Encode video to 360p

function encode360p
    set input_file $argv[1]
    set base_name (basename -s .mp4 $input_file)
    set output_file "$base_name-p360.mp4"
    ffmpeg -i $input_file -vf scale=640:360 -c:a copy -c:v libx264 -crf 23 $output_file
end

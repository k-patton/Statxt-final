for file in *.mc; do
    mv "$file" "$(basename "$file" .mc).stxt"
done

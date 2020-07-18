zip_file=download.zip

# download contains a zip with all cards as svg files (if it doesn't already exist)
if [ ! -f "$zip_file" ]; then
curl 'https://www.me.uk/cards/makeadeck.cgi' \
  -H 'Connection: keep-alive' \
  -H 'Cache-Control: max-age=0' \
  -H 'Upgrade-Insecure-Requests: 1' \
  -H 'Origin: https://www.me.uk' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.89 Safari/537.36' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
  -H 'Sec-Fetch-Site: same-origin' \
  -H 'Sec-Fetch-Mode: navigate' \
  -H 'Sec-Fetch-User: ?1' \
  -H 'Sec-Fetch-Dest: document' \
  -H 'Referer: https://www.me.uk/cards/makeadeck.cgi' \
  -H 'Accept-Language: de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7' \
  --data-raw 'size=poker&back=Diamond&ace=Fancy&ace1=elm-spider-solitaire&ace2=&duplimate=&qr=https%3A%2F%2Fde-flwi-elm-spider-solitaire.netlify.app&backcolour=%23ffffff&frontcolour=%23ffffff&blackcolour=%23000000&redcolour=%23ff0000&value=0&pip=1&zip=Download+zip+file' \
  --compressed \
  > "$zip_file"

fi

# extract rank svg-paths
unzip -p $zip_file \
| npx svgson \
| jq '.[].children[] | select(.name == "symbol") | select(.attributes.id | test("V.*")) | { (.attributes.id[2:3]): ( .children[] | select( .name == "path") | .attributes.d ) }' \
| jq -s -S add \
> ranks.json

# extract suit svg-paths
unzip -p $zip_file \
| npx svgson \
| jq '.[].children[] | select(.name == "symbol") | select(.attributes.id | test("S[CDSH].")) | { (.attributes.id[1:2]): ( .children[] | select( .name == "path") | .attributes.d ) }' \
| jq -s -S add \
> suits.json





# just for reference:
# handle single files out of zip-archive

# for filename in $(zipinfo -1 $zip_file); do
#   json_file=$(echo "$filename" | sed -e 's/svg/json/g')
#
#   svg_content=$(unzip -p $zip_file $filename)
#
# done

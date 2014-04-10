rm recschedule*

# download schedule
wget http://web.mit.edu/athletics/www/recschedule.pdf -O ~/recschedule.pdf

# convert to text
pdftotext -layout ~/recschedule.pdf ~/recschedule.txt

# extract Badminton records with dates and remove others
# then print all lines after current day
cat ~/recschedule.txt | grep -B 1 'day\|Badminton' | grep -v "Start\|Volleyball\|Basketball" | grep -A 500 $(date +%A)

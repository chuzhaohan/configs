rm recschedule*

# download schedule
wget http://web.mit.edu/athletics/www/recschedule.pdf

# convert to text
pdftotext -layout ~/recschedule.pdf ~/recschedule.txt

# extract Badminton records with dates and remove others
cat recschedule.txt | grep -B 1 'day\|Badminton' | grep -v "Start\|Volleyball\|Basketball"

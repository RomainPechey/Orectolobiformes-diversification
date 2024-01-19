while read line; do
  Genus=$(echo "$line" |cut -d"," -f1| awk '{print $1}')
  Species=$(echo "$line" |cut -d"," -f2|sed -e 's:^M::g'|sed -e 's:\r::g'| awk '{print $2}')
  arg=$(echo "https://fishbase.mnhn.fr/map/downloadKML.php?Spname=")
  arg+=$(echo "$Genus")
  arg+=$(echo "+")
  arg+=$(echo "$Species")
  echo $arg
  outfile=$(echo "$Genus")
  outfile+=$(echo "_")
  outfile+=$(echo "$Species")
  outfile2=$(echo "$Genus")
  outfile2+=$(echo "_")
  outfile2+=$(echo "$Species")
  outfile2+=$(echo ".txt")
  wget -O $outfile $arg
  echo "$Genus,$Species" >> $outfile2
  cat   $outfile| grep -i "<Point><coordinates>"|sed -e 's:<Point><coordinates>::g'|sed -e 's:</coordinates></Point>::g'|sed -e 's:\t::g' >> $outfile2

done < Species_list.txt
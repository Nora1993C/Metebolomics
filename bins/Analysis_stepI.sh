Data=/Users/caiyuanyuan/Desktop/HFQ黄丰青/202107胆汁;

cd $Data;
### Distance
[ ! -d $Data/1.2_raw_Distance ] && mkdir $Data/1.2_raw_Distance;
for i in $(ls $Data/0.0_raw_data/*.data.txt)
do
	ID=$(basename $i);
	Rscript $Data/bin/A1.1_Calculate_Distance.r $i $Data/1.2_raw_Distance;
done
###PCA plot
[ ! -d $Data/1.3_raw_PCA ] && mkdir $Data/1.3_raw_PCA;
for i in $(ls $Data/1.2_raw_Distance/*.txt)
do
        ID=$(basename $i);
        Rscript $Data/bin/A1.2_Plot_pca.r $i $Data/bin/${ID%.*.*}-grouping.info $Data/1.3_raw_PCA 2
done


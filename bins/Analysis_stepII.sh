#Analysis StepI: Extract Significant data 

Data=/Users/caiyuanyuan/Desktop/HFQ黄丰青/202107胆汁
cd $Data;

### Extract signficant data between groups using U test or KW test
[ ! -d $Data/2.1_Sig_KWU ] && mkdir $Data/2.1_Sig_KWU;
for i in $(ls $Data/0.0_raw_data/*.data.txt)
do
        ID=$(basename $i);
        Rscript $Data/bin/A2.1_Extract_Sig_KWU.r $i $Data/bin/${ID%.*.*}-grouping.info $Data/2.1_Sig_KWU 2;
        #Rscript $Data/bin/A2.1_Extract_Sig_t.r $i $Data/0.0_raw_data/${ID%.*.*}-grouping.info $Data/2.1_Sig_KWU 2;
done

for i in $(ls $Data/2.1_Sig_KWU/*Sig.data.txt);
do
        perl -pi -e "s/^\t/ID\t/g" $i;
done


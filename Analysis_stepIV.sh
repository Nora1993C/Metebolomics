#Analysis StepV: analysis for VIP and significant sites


Data=/Users/caiyuanyuan/Desktop/HFQ黄丰青/202107胆汁
cd $Data;

#Extract signficant data from VIP sites(OPLS-DA)
[ ! -d $Data/4.1_VIP_Sig ] && mkdir $Data/4.1_VIP_Sig   ;
for i in $(ls $Data/2.1_Sig_KWU/*fdr.Sig.data.txt);
do
        ID=$(basename $i);
        perl $Data/bin/A4.1_Extract_VIP_SIG.pl $i  $Data/3.2_OPLS_DA/${ID%.*.*.*.*}.OPLSDA.VIP.txt $Data/4.1_VIP_Sig/${ID%.*.*}.VIP.txt;
done

for i in $(ls $Data/4.1_VIP_Sig/*VIP.txt);
do
        perl -pi -e "s/^ID\t/name\t/g" $i;
done

#Extract rev data
#for i in $(ls $Data/0.0_raw_data/*CM[A-Z]*.data.txt);
#do
#        ID=$(basename $i);
#        Rscript $Data/bin/A4.1_Sig_U_rev.r $i   $Data/0.0_raw_data/${ID%.*.*}-grouping.info   $Data/4.1_VIP_Sig   2   $Data/3.2_OPLS_DA/${ID%CM[A-Z]*.*.*}CM.OPLSDA.VIP.txt ;
#        Rscript $Data/bin/A4.2_Sig_T_rev.r $i   $Data/0.0_raw_data/${ID%.*.*}-grouping.info   $Data/4.1_VIP_Sig   2   $Data/3.2_OPLS_DA/${ID%CM[A-Z].*.*}CM.OPLSDA.VIP.txt ;
#done

#Convert mz data to HMDB_ID, to Gene_Name
[ ! -d $Data/4.7_VIP_Sig_anno ] && mkdir $Data/4.7_VIP_Sig_anno;
for i in $(ls $Data/4.1_VIP_Sig/pos*.txt)
do
	ID=$(basename $i);
	perl $Data/bin/A4.7_Extract_VIP_SIG_MZ.pl $i $Data/0.0_raw_data/pos.mz.dat $Data/4.7_VIP_Sig_anno/${ID%.*}.mz.dat;
	perl $Data/bin/A4.8_MZ_HMDB_Gene.pl $Data/4.7_VIP_Sig_anno/${ID%.*}.mz.dat $Data/4.7_VIP_Sig_anno;
done

for i in $(ls $Data/4.1_VIP_Sig/neg*.txt)
do
	ID=$(basename $i);
	perl $Data/bin/A4.7_Extract_VIP_SIG_MZ.pl $i $Data/0.0_raw_data/neg.mz.dat $Data/4.7_VIP_Sig_anno/${ID%.*}.mz.dat;
	perl $Data/bin/A4.8_MZ_HMDB_Gene.pl $Data/4.7_VIP_Sig_anno/${ID%.*}.mz.dat $Data/4.7_VIP_Sig_anno;
done

#Analysis StepV: analysis for VIP and significant sites
Data=/Users/caiyuanyuan/Desktop/HFQ黄丰青/2021补阳还五汤脂质组

cd $Data;

#Extract signficant data from VIP sites(OPLS-DA)
[ ! -d $Data/4.1_VIP_Sig ] && mkdir $Data/4.1_VIP_Sig   ;
for i in $(ls $Data/2.1_Sig_KWU/*Sig.data.txt);
do
        ID=$(basename $i);
        perl $Data/bin/A4.1_Extract_VIP_SIG.pl $i  $Data/3.2_OPLS_DA/${ID%.*.*.*.*}.OPLSDA.VIP.txt $Data/4.1_VIP_Sig/${ID%.*.*}.VIP.txt;
done

#Extract rev data
for i in $(ls $Data/0.0_raw_data/*CM[A-Z].data.txt);
do
        ID=$(basename $i);
        Rscript $Data/bin/A4.1_Sig_U_rev.r $i   $Data/0.0_raw_data/${ID%.*.*}-grouping.info   $Data/4.1_VIP_Sig   2   $Data/3.2_OPLS_DA/${ID%CM[A-Z].*.*}CM.OPLSDA.VIP.txt ;
        Rscript $Data/bin/A4.2_Sig_T_rev.r $i   $Data/0.0_raw_data/${ID%.*.*}-grouping.info   $Data/4.1_VIP_Sig   2   $Data/3.2_OPLS_DA/${ID%CM[A-Z].*.*}CM.OPLSDA.VIP.txt ;
done

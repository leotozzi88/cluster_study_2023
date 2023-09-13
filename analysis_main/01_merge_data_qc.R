setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

subs_qc=c('60100007', '60100018', '60100029', '60100030', '60100040', '60100052', '60100063', '60100074', '60100085', '60100108', '60100119', '60100120', '60100131', '60100142', '60100153', '60100164', '60100175', '60100186', '60100197', '60100209', '60100210', '60100221', '60100232', '60100243', '60100254', '60100265', '60100276', '60100298', '60100300', '60100311', '60100322', '60100333', '60100344', '60100355', '60100366', '60100377', '60100388', '60100399', '60100401', '60100412', '60100423', '60100445', '60100456', '60100467', '60100478', '60100489', '60100502', '60100513', '60100524', '60100535', '60100546', '60100557', '60100580', '60100591', '60100614', '60100625', '60100636', '60100647', '60100658', '60100669', '60100670', '60100681', '60100692', '60100715', '60100726', '60100759', '60100805', '60100816', '60100827', '60100849', '60100861', '60100894', '60100906', '60100917', '60100940', '60101020', '60101031', '60101075', '60101086', '60101097', '60101109', '60101121', '60101154', '60101165', '60101187', '60101200', '60101244', '60101255', '60101266', '60101299', '60101301', '60101312', '60101323', '60101334', '60101356', '60101367', '60101378', '60101389', '60101402', '60101413', '60101435', '60101446', '60101457', '60101468', '60101479', '60101480', '60101491', '60101503', '60101514', '60101525', '60101536', '60101547', '60101558', '60101569', '60101570', '60101581', '60101592', '60101615', '60101626', '60101637', '60101648', '60101659', '60101660', '60101693', '60101705', '60101738', '60101750', '60101772', '60101895', '60101907', '60101918', '60101963', '60101974', '60101985', '60102010', '60102021', '60102032', '60102043', '60102133', '60102155', '60102177', '60102188', '60102199', '60102212', '60102223', '60102234', '60102256', '60102267', '60102278', '60102289', '60102302', '60102324', '60102335', '60102357', '60102368', '60102379', '60102380', '60102425', '60102469', '60102470', '60102504', '60102526', '60102548', '60102559', '60102560', '60102627', '60102661', '60102717', '60102773', '60102784', '60102795', '60102818', '60102830', '60102841', '60102852', '60102863', '60102885', '60102896', '60102908', '60102919', '60102920', '60102931', '60102942', '60102953', '60102975', '60102986', '60102997', '60103000', '60103011', '60103044', '60103134', '60103145', '60103190', '60103213', '60103268', '60103314', '60103347', '60103358', '60103381', '60103392', '60103426', '60103448', '60103482', '60103493', '60103561', '60103583', '60103594', '60103639', '60103684', '60103695', '60103707', '60103763', '60103774', '60103785', '60103796', '60103819', '60103820', '60103831', '60103842', '60103853', '60103864', '60103897', '60103909', '60103910', '60103921', '60103943', '60103954', '60103976', '60104012', '60104034', '60104067', '60104078', '60104090', '60104113', '60104135', '60104157', '60104236', '60104247', '60104258', '60104269', '60104270', '60104326', '60104337', '60104348', '60105024', '60105035', '60105046', '60105068', '60105079', '60105080', '60105091', '60105103', '60105114', '60105125', '60105136', '60105147', '60105158', '60105169', '60105170', '60105181', '60105192', '60105204', '60105215', '60105226', '60105237', '60105248', '60105259', '60105271', '60105282', 
          'CONN010', 'CONN011', 'CONN013', 'CONN014', 'CONN015', 'CONN016', 'CONN017', 'CONN018', 'CONN019', 'CONN020', 'CONN021', 'CONN022', 'CONN023', 'CONN024', 'CONN025', 'CONN026', 'CONN027', 'CONN028', 'CONN029', 'CONN030', 'CONN031', 'CONN032', 'CONN033', 'CONN034', 'CONN035', 'CONN036', 'CONN037', 'CONN038', 'CONN039', 'CONN040', 'CONN041', 'CONN042', 'CONN043', 'CONN044', 'CONN045', 'CONN046', 'CONN047', 'CONN048', 'CONN049', 'CONN050', 'CONN051', 'CONN052', 'CONN053', 'CONN054', 'CONN055', 'CONN056', 'CONN057', 'CONN058', 'CONN059', 'CONN060', 'CONN061', 'CONN062', 'CONN063', 'CONN064', 'CONN065', 'CONN066', 'CONN067', 'CONN068', 'CONN069', 'CONN070', 'CONN071', 'CONN072', 'CONN073', 'CONN074', 'CONN075', 'CONN076', 'CONN077', 'CONN078', 'CONN079', 'CONN080', 'CONN082', 'CONN083', 'CONN084', 'CONN102', 'CONN103', 'CONN104', 'CONN105', 'CONN107', 'CONN108', 'CONN110', 'CONN111', 'CONN112', 'CONN113', 'CONN114', 'CONN115', 'CONN116', 'CONN117', 'CONN118', 'CONN119', 'CONN120', 'CONN121', 'CONN122', 'CONN124', 'CONN125', 'CONN126', 'CONN127', 'CONN128', 'CONN129', 'CONN130', 'CONN131', 'CONN132', 'CONN133', 'CONN134', 'CONN135', 'CONN136', 'CONN137', 'CONN139', 'CONN140', 'CONN141', 'CONN142', 'CONN143', 'CONN144', 'CONN145', 'CONN146', 'CONN147', 'CONN148', 'CONN149', 'CONN150', 'CONN151', 'CONN152', 'CONN153', 'CONN154', 'CONN155', 'CONN156', 'CONN157', 'CONN158', 'CONN159', 'CONN160', 'CONN161', 'CONN162', 'CONN163', 'CONN166', 'CONN167', 'CONN168', 'CONN169', 'CONN170', 'CONN171', 'CONN172', 'CONN173', 'CONN174', 'CONN175', 'CONN176', 'CONN177', 'CONN178', 'CONN179', 'CONN180', 'CONN181', 'CONN182', 'CONN183', 'CONN185', 'CONN186', 'CONN188', 'CONN189', 'CONN190', 'CONN191', 'CONN192', 'CONN193', 'CONN194', 'CONN195', 'CONN196', 'CONN197', 'CONN198', 'CONN199', 'CONN200', 'CONN202', 'CONN203', 'CONN204', 'CONN206', 'CONN207', 'CONN208', 'CONN209', 'CONN210', 'CONN211', 'CONN212', 'CONN213', 'CONN215', 'CONN216', 'CONN217', 'CONN219', 'CONN220', 'CONN221', 'CONN222', 'CONN223', 'CONN224', 'CONN225', 'CONN226', 'CONN227', 'CONN228', 'CONN230', 'CONN231', 'CONN232', 'CONN234', 'CONN235', 'CONN236', 'CONN245', 'CONN246', 'CONN247', 'CONN248', 'CONN250', 'CONN251', 'CONN252', 'CONN254', 'CONN255', 'CONN257', 'CONN259', 'CONN260', 'CONN261', 'CONN262', 'CONN263', 'CONN264', 'CONN267', 'CONN268', 'CONN269', 'CONN270', 'CONN271', 'CONN272', 'CONN273', 'CONN274', 'CONN275', 'CONN276', 'CONN277', 'CONN278', 'CONN279', 'CONN280', 'CONN281', 'CONN282', 'CONN284', 'CONN285', 'CONN286', 'CONN287', 'CONN288', 'CONN289', 'CONN290', 'CONN294', 'CONN296', 'CONN298', 'CONN299', 'CONN300', 'CONN301', 'CONN302', 'CONN303', 'CONN304', 'CONN306', 'CONN308', 'CONN309', 'CONN310', 'CONN311', 'CONN312', 'CONN314', 'CONN315', 'CONN317', 'CONN318', 'CONN319', 'CONN320', 'CONN321', 'CONN322', 'CONN323', 'CONN326', 'CONN327', 'CONN329', 'CONN330', 'CONN331', 'CONN332', 'CONN333', 'CONN335', 'CONN336', 'CONN338', 'CONN339', 'CONN340', 'CONN341', 'CONN342', 'CONN343', 'CONN344', 'CONN345', 'CONN346', 'CONN348', 'CONN350', 'CONN351', 'CONN352', 'CONN353', 'CONN354', 'CONN355', 'CONN356', 'CONN360', 
          'LA13272', 'LA14016', 'MV00878', 'MV00962', 'MV00992', 'MV01113', 'MV01438', 'MV01836', 'MV01950', 'MV04661', 'MV05158', 'MV05953', 'MV06084', 'MV07296', 'MV07303', 'MV07572', 'MV07647', 'MV08032', 'MV08112', 'MV08176', 'MV08645', 'MV08712', 'MV08866', 'MV09122', 'MV09305', 'MV09434', 'MV09441', 'MV09560', 'MV09586', 'MV09876', 'MV11065', 'MV11133', 'MV11135', 'MV11150', 'MV11202', 'PA20147', 'PA21728', 'PA22014', 'PA22518', 'PA22544', 'PA22561', 'PA22568', 'PA22594', 'PA22725', 'PA22728', 'PA22772', 'PA23284', 'PA23955', 'PA24195', 'PA24326', 'PA24603', 'PA24859', 'PA24876', 'PA25084', 'PA25119', 'PA25306', 'PA25642', 'PA25692', 'PA25870', 'PA25894', 'PA25960', 'PA25994', 'PA26039', 'PA26203', 'PA26376', 'PA26650', 'PA27040', 'PA27394', 'PA27434', 'PA27493', 'PA27541', 'PA27578', 'PA27793', 'PA27962', 'PA27995', 'PA28033', 'PA28219', 'PA28336', 'PA28460', 'PA28464', 'PA28564', 'PA28985', 'PA28989', 'PA29385', 'PA29661', 'PA29685', 'PA29689', 'PA30071', 'PA30104', 'PA30563', 'PA30677', 'PA30861', 'PA30862', 'SU30816', 'SU33550', 'SU35282',
          'RAD001', 'RAD002', 'RAD003', 'RAD004', 'RAD005', 'RAD006', 'RAD007', 'RAD008', 'RAD009', 'RAD010', 'RAD013', 'RAD014', 'RAD015', 'RAD016', 'RAD017', 'RAD018', 'RAD019', 'RAD020', 'RAD022', 'RAD023', 'RAD024', 'RAD025', 'RAD026', 'RAD028', 'RAD029', 'RAD030', 'RAD031', 'RAD032', 'RAD033', 'RAD034', 'RAD035', 'RAD036', 'RAD037', 'RAD038', 'RAD039', 'RAD040', 'RAD041', 'RAD042', 'RAD043', 'RAD044', 'RAD045', 'RAD046', 'RAD047', 'RAD048', 'RAD049', 'RAD050', 'RAD051', 'RAD052', 'RAD053', 'RAD054', 'RAD055', 'RAD056', 'RAD057', 'RAD058', 'RAD059', 'RAD060', 'RAD061', 'RAD062', 'RAD063', 'RAD064', 'RAD065', 'RAD066', 'RAD067', 'RAD068', 'RAD069', 'RAD070', 'RAD071', 'RAD072', 'RAD073', 'RAD074', 'RAD075', 'RAD076', 'RAD077', 'RAD078', 'RAD079', 'RAD080', 'RAD081', 'RAD082', 'RAD083', 'RAD084', 'RAD085', 'RAD086', 'RAD087', 'RAD088', 'RAD089', 'RAD090', 'RAD091', 'RAD092', 'RAD093', 'RAD095', 'RAD096', 'RAD097', 'RAD098', 'RAD099', 'RAD100', 'RAD101', 'RAD102', 'RAD103', 'RAD104', 'RAD105', 'RAD106', 'RAD107', 'RAD108', 'RAD109', 'RAD110', 'RAD111', 'RAD112', 'RAD113', 'RAD114', 'RAD115', 'RAD116', 'RAD117', 'RAD118', 'RAD119', 'RAD120', 'RAD121', 'RAD124', 'RAD125', 'RAD126', 'RAD127', 'RAD128', 'RAD129', 'RAD130', 'RAD132', 'RAD133', 'RAD135', 'RAD136', 'RAD137', 'RAD138', 'RAD139', 'RAD140', 'RAD141', 'RAD142', 'RAD143', 'RAD144', 'RAD145', 'RAD146', 'RAD147', 'RAD148', 'RAD149', 'RAD150', 'RAD151', 'RAD152', 'RAD153', 'RAD154', 'RAD155', 'RAD156', 'RAD157', 'RAD158', 'RAD159', 'RAD160', 'RAD161', 'RAD162', 'RAD164', 'RAD165', 'RAD166', 'RAD167', 'RAD168', 'RAD169', 'RAD170', 'RAD171', 'RAD172', 'RAD173', 'RAD174', 'RAD175', 'RAD176', 'RAD177', 'RAD179', 'RAD181', 'RAD182', 'RAD183', 'RAD184', 'RAD186', 'RAD187', 'RAD188', 'RAD189', 'RAD190', 'RAD191', 'RAD192', 'RAD193', 'RAD194', 'RAD195', 'RAD196', 'RAD197', 'RAD198', 'RAD199', 'RAD200', 'RAD201', 'RAD202', 'RAD203', 'RAD204', 'RAD205', 'RAD206', 'RAD207', 'RAD208', 'RAD209', 'RAD210', 'RAD211', 'RAD212', 'RAD213', 'RAD214', 'RAD215', 'RAD216', 'RAD217', 'RAD218', 'RAD221', 'RAD223', 'RAD224', 'RAD226', 'RAD227', 'RAD229', 'RAD230', 'RAD231', 'RAD233', 'RAD234', 'RAD235', 'RAD236', 'RAD237', 'RAD238', 'RAD239', 'RAD240', 'RAD241', 'RAD246', 'RAD247', 'RAD248', 'RAD250', 'RAD251', 'RAD255', 'RAD256', 'RAD259', 'RAD261', 'RAD264', 'RAD265', 'RAD266', 'RAD267', 'RAD268', 'RAD270', 'RAD271', 'RAD272', 'RAD273', 'RAD279', 'RAD287', 'RAD292', 'RAD295', 'RAD296', 'RAD298', 'RAD299', 'RAD300', 'RAD304', 'RAD305', 'RAD306', 'RAD307', 'RAD309', 'RAD310', 'RAD311', 'RAD312', 'RAD313', 'RAD317', 'RAD320', 'RAD321', 'RAD322', 'RAD324', 'RAD325', 'RAD326', 'RAD329', 'RAD333', 'RAD334', 'RAD335', 'RAD336', 'RAD337', 'RAD338', 'RAD339', 'RAD340', 'RAD341', 'RAD342', 'RAD344', 'RAD345', 'RAD346', 'RAD347', 'RAD349', 'RAD350', 'RAD351', 'RAD352', 'RAD353', 'RAD355', 'RAD356', 'RAD357', 'RAD359', 'RAD361', 'RAD362', 'RAD364', 'RAD365', 'RAD367', 'RAD369', 'RAD370', 'RAD371', 'RAD372', 'RAD377', 'RAD378', 'RAD379', 'RAD381', 'RAD389', 'RAD390', 'RAD392', 'RAD394', 'RAD395', 'RAD396', 'RAD397', 'RAD401', 'RAD402', 'RAD403', 'RAD404', 'RAD405', 'RAD407', 'RAD412', 'RAD413', 'RAD416')


fd=read.csv('data/fd_table.csv')
img=read.csv('data/circ_table_mo6wmcsf_denoised.csv')
ques=read.csv('data/quest_rad_ispotd_fu_hcpdes_engage_clean.csv')
beh=read.csv('data/beh_rad_ispotd_hcpdes_engage_clean_inwn.csv')

# Merge integneuro and webneuro
library(dplyr)
beh <- beh %>%
  mutate(
    wn_emzcompk_norm = ifelse(is.na(wn_emzcompk_norm), in_emzcmpin_norm, wn_emzcompk_norm),
    wn_emzerrk_norm = ifelse(is.na(wn_emzerrk_norm), in_emzerr_norm, wn_emzerrk_norm),
    wn_g2avrtk_norm = ifelse(is.na(wn_g2avrtk_norm), in_gngavrt_norm, wn_g2avrtk_norm),
    wn_g2fpk_norm = ifelse(is.na(wn_g2fpk_norm), in_gngfp_norm, wn_g2fpk_norm),
    wn_wmfnk_norm = ifelse(is.na(wn_wmfnk_norm), in_wmfn_norm, wn_wmfnk_norm),
    wn_wmfpk_norm = ifelse(is.na(wn_wmfpk_norm), in_wmfp_norm, wn_wmfpk_norm),
    wn_wmrtk_norm = ifelse(is.na(wn_wmrtk_norm), in_wmrt_norm, wn_wmrtk_norm),
    wn_gettrth_norm = ifelse(is.na(wn_gettrth_norm), in_getcrth_norm, wn_gettrth_norm),
    wn_gettrta_norm = ifelse(is.na(wn_gettrta_norm), in_getcrta_norm, wn_gettrta_norm),
    wn_gettrtf_norm = ifelse(is.na(wn_gettrtf_norm), in_getcrtf_norm, wn_gettrtf_norm),
    wn_gettrts_norm = ifelse(is.na(wn_gettrts_norm), in_getcrts_norm, wn_gettrts_norm), 
    wn_dgttrth_norm  = ifelse(is.na(wn_dgttrth_norm), in_cdsgcrth_norm, wn_dgttrth_norm), 
    wn_dgttrtf_norm  = ifelse(is.na(wn_dgttrtf_norm), in_cdsgcrtf_norm, wn_dgttrtf_norm), 
    wn_dgttrts_norm  = ifelse(is.na(wn_dgttrts_norm), in_cdsgcrts_norm, wn_dgttrts_norm), 
    wn_dgttrta_norm  = ifelse(is.na(wn_dgttrta_norm), in_cdsgcrta_norm, wn_dgttrta_norm), 
    wn_dgttrtn_norm  = ifelse(is.na(wn_dgttrtn_norm), in_cdsgcrtn_norm, wn_dgttrtn_norm), 
  )



# Remove scans with high FD
for (sub in img$id){
  if (sub %in% fd$id){
  if (fd[fd$id==sub,'fd03_gng']>0.25 & !is.na(fd[fd$id==sub,'fd03_gng'])){
    img[img$id==sub,grep("gng", names(img), value=TRUE)]=NA
  }
  if (fd[fd$id==sub,'fd03_con']>0.25 & !is.na(fd[fd$id==sub,'fd03_con'])){
    img[img$id==sub,grep("con", names(img), value=TRUE)]=NA
  }
  if (fd[fd$id==sub,'fd03_nco']>0.25 & !is.na(fd[fd$id==sub,'fd03_nco'])){
    img[img$id==sub,grep("nco", names(img), value=TRUE)]=NA
  }
  if (fd[fd$id==sub,'fd03_rest']>0.25 & !is.na(fd[fd$id==sub,'fd03_rest'])){
    img[img$id==sub,grep("ic", names(img), value=TRUE)]=NA
  }
  }
}

write.csv(img, 'data/circ_table_fdremoved.csv', row.names = FALSE)

# Merge imaging and questionnaires
merged=merge(ques, img, by = 'id')

# Merge with behavior
merged=merge(merged, beh, by = 'id', all.x = TRUE)

# Save data
write.csv(merged, 'data/dataset_merged.csv', row.names = FALSE)

# Keep only subjects who passed QC
merged_qc=merged[merged$id %in% subs_qc, ]

# Keep only subjects with imaging
img_vars=c('ic.791103_Left_AG.to.752691_Left_amPFC', 'ic.752691_Left_amPFC.to.414207_Right_AG', 'ic.752691_Left_amPFC.to.073720_Medial_PCC', 'ic.791103_Left_AG.to.073720_Medial_PCC', 'ic.414207_Right_AG.to.073720_Medial_PCC', 'ic.477522_Left_antInsula.to.447647_Left_Amygdala', 'ic.954861_Right_antInsula.to.450767_Right_Amygdala', 'ic.477522_Left_antInsula.to.954861_Right_antInsula', 'act.con.sad_vs_neu.779062_Left_Amygdala', 'act.con.sad_vs_neu.176064_Right_Amygdala', 'act.con.sad_vs_neu.707656_Left_antInsula', 'act.con.sad_vs_neu.534482_Right_antInsula', 'act.con.sad_vs_neu.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.707656_Left_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.707656_Left_antInsula', 'ppi.con.sad_vs_neu.534482_Right_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.534482_Right_antInsula', 'ppi.con.sad_vs_neu.779062_Left_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.779062_Left_Amygdala', 'ppi.con.sad_vs_neu.176064_Right_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.176064_Right_Amygdala', 'act.con.thr_vs_neu.779062_Left_Amygdala', 'act.con.thr_vs_neu.176064_Right_Amygdala', 'act.con.thr_vs_neu.131520_Medial_dACC', 'ppi.con.thr_vs_neu.779062_Left_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.779062_Left_Amygdala', 'ppi.con.thr_vs_neu.176064_Right_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.176064_Right_Amygdala', 'act.nco.thr_vs_neu.779062_Left_Amygdala', 'act.nco.thr_vs_neu.176064_Right_Amygdala', 'act.nco.thr_vs_neu.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.779062_Left_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.779062_Left_Amygdala', 'ppi.nco.thr_vs_neu.176064_Right_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.176064_Right_Amygdala', 'act.con.happy_vs_neu.182355_Medial_mOFC', 'act.con.happy_vs_neu.001716_Left_vStriatum', 'act.con.happy_vs_neu.877949_Right_vStriatum', 'ic.532911_Left_lPFC.to.156999_Left_msPFC', 'ic.275836_Right_lPFC.to.156999_Left_msPFC', 'ic.125909_Left_aIPL.to.532911_Left_lPFC', 'ic.525931_Right_aIPL.to.275836_Right_lPFC', 'ic.125909_Left_aIPL.to.831650_Left_precuneus', 'ic.525931_Right_aIPL.to.216192_Right_precuneus', 'act.gng.nogo_vs_go.013136_Left_dlPFC', 'act.gng.nogo_vs_go.533294_Right_dlPFC', 'act.gng.nogo_vs_go.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.013136_Left_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.013136_Left_dlPFC', 'ppi.gng.nogo_vs_go.533294_Right_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.533294_Right_dlPFC')
merged_qc=merged_qc[rowSums(is.na(merged_qc[, img_vars]))<length(img_vars), ]

#### Variables for demographics table #### 

# Merge RAD clinical sub-samples
merged_qc$study_reduced=merged_qc$study
merged_qc[merged_qc$study %in% c('RAD_generalizability', 'RAD_main', 'RAD_medicated'), 'study_reduced']='RAD'

# Summarize data sets for tables
merged_qc[merged_qc$study_reduced=='RAD', 'table_sample']=1
merged_qc[merged_qc$study_reduced=='HCPDES' & merged_qc$group==1, 'table_sample']=2
merged_qc[merged_qc$study_reduced=='ISPOTD' & !is.na(merged_qc$treatment_arm) & merged_qc$treatment_arm=='', 'table_sample']=3
merged_qc[merged_qc$study_reduced=='HCPDES' & merged_qc$group==0, 'table_sample']=4
merged_qc[merged_qc$study_reduced=='ISPOTD' & merged_qc$group==0, 'table_sample']=5
merged_qc[merged_qc$study_reduced=='ISPOTD' & !is.na(merged_qc$treatment_arm) & merged_qc$treatment_arm=='Escitalopram', 'table_sample']=6
merged_qc[merged_qc$study_reduced=='ISPOTD' & !is.na(merged_qc$treatment_arm) & merged_qc$treatment_arm=='Sertraline', 'table_sample']=7
merged_qc[merged_qc$study_reduced=='ISPOTD' & !is.na(merged_qc$treatment_arm) & merged_qc$treatment_arm=='Venlafaxine XR', 'table_sample']=8
merged_qc[merged_qc$study_reduced=='ENGAGE' & merged_qc$treatment_arm=='TAU', 'table_sample']=9
merged_qc[merged_qc$study_reduced=='ENGAGE' & merged_qc$treatment_arm=='I-CARE', 'table_sample']=10

merged_qc[merged_qc$study_reduced=='RAD', 'table_sample']=1
merged_qc[merged_qc$study_reduced=='HCPDES' & merged_qc$group==1, 'table_sample']=2
merged_qc[merged_qc$study_reduced=='ISPOTD' & merged_qc$group==1, 'table_sample']=3
merged_qc[merged_qc$study_reduced=='ENGAGE', 'table_sample']=4
merged_qc[merged_qc$study_reduced=='HCPDES' & merged_qc$group==0, 'table_sample']=5
merged_qc[merged_qc$study_reduced=='ISPOTD' & merged_qc$group==0, 'table_sample']=6

# Save data
write.csv(merged_qc, 'data/dataset_merged_qc.csv', row.names = FALSE)



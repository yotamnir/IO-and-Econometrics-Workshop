import delimited "C:\Users\user\Dropbox\תשפא\IO and Econometrics Workshop\IO-and-Econometrics-Workshop\Final Project\Monthly 2018-2021.csv", encoding(UTF-8) clear 

replace degemnm = "7NBHW6/3" if degemnm == "3/6WHBN7"
replace degemnm = "7JBHYM/3PS" if degemnm == "SP3/MYHBJ7"
replace degemnm = "7NBHY6/3" if degemnm == "3/6YHBN7"
***replace degemnm = "CUYHXN" if degemnm == "CUYHXN/1S"				REPLACED IN PRICES DATASET
replace degemnm = "MDZ642L-LBADGW" if degemnm == "WGDABL-L246ZDM"
replace degemnm = "MDZ342L-LBZDGW" if degemnm == "WGDZBL-L243ZDM"
replace degemnm = "MDZ662L-LBADGW" if degemnm == "WGDABL-L266ZDM"
replace degemnm = "AHKH-B2B01E" if degemnm == "E10B2B-HKHA"
replace degemnm = "BHXH-B2B01C" if degemnm == "C10B2B-HXHB"
replace degemnm = "GUN125L-DTTMHW" if degemnm == "WHMTTD-L521NUG"
replace degemnm = "GUN125L-DTTSHW" if degemnm == "WHSTTD-L521NUG"
replace degemnm = "GUN125L-DTFSHW" if degemnm == "WHSFTD-L521NUG"
replace degemnm = "GUN135L-DTTMHW" if degemnm == "WHMTTD-L531NUG"
replace degemnm = "DJ6HV" if degemnm == "VH6JD"
replace degemnm = "ZWE186L-DWXNBW" if degemnm == "WBNXWD-L681EWZ"
replace degemnm = "ZWE186L-DHXSBW" if degemnm == "WBSXHD-L681EWZ"
replace degemnm = "ZWE186L-DWXSBW" if degemnm == "WBSXWD-L681EWZ"
replace degemnm = "ZWE186L-DWXGBW" if degemnm == "WBGXWD-L681EWZ"
replace degemnm = "ZWE186L-DHXGBW" if degemnm == "WBGXHD-L681EWZ"
replace degemnm = "HN516G" if degemnm == "G615NH"
replace degemnm = "HM816A" if degemnm == "A618MH"
replace degemnm = "FK78" if degemnm == "87KF"
replace degemnm = "HSDJ9P" if degemnm == "P9JDSH"
replace degemnm = "5SDL1A" if degemnm == "A1LDS5"
replace degemnm = "5SDKJC" if degemnm == "CJKDS5"
replace degemnm = "PG81AB" if degemnm == "BA18GP" | degemnm == "PG81Aע" | degemnm == "PG81A"
replace degemnm = "GSL30L-PRZQHA" if degemnm == "AHQZRP-L03LSG"
replace degemnm = "GSL30L" if degemnm == "L03LSG"
replace degemnm = "952ABA2" if degemnm == "2ABA259"
replace degemnm = "949AXF2A" if degemnm == "A2FXA949"
replace degemnm = "225AXG11" if degemnm == "11GXA522"
replace degemnm = "CCHNZT/S" if degemnm == "S/TZNHCC"
replace degemnm = "0SDCJ5" if degemnm == "OSDCJ5"
replace degemnm = "GSU55L-ARZNHA" if degemnm == "AHNZRA-L55USG"
replace degemnm = "GSU75L-ARZ" if degemnm == "ZRA-L57USG"
replace degemnm = "AXUH78L-ARXMHA" if degemnm == "AHMXRA-L87HUXA"
replace degemnm = "9YA" if degemnm == "AY9"
replace degemnm = "117.342" if degemnm == "243.711"
replace degemnm = "" if degemnm == ""
replace degemnm = "" if degemnm == ""

g date2 = date(date, "DMY")
g year = year(date2)
g month = day(date2)

duplicates tag degemcd degemnm shnatyitzur sugdegem tozeretcd year month, gen(tag)

merge m:1 degemcd degemnm shnatyitzur sugdegem tozeretcd using "C:\Users\user\Dropbox\תשפא\IO and Econometrics Workshop\IO-and-Econometrics-Workshop\Final Project\Prices.dta"
	sort shnatyitzur tozeretcd degemcd sugdegem

drop if kinuymishari == "2008" & shnatyitzur == 2017	// none in master
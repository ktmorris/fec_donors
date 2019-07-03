### THIS CODE SCRAPES NYS PAROLE WEBSITE TO CHECK FOR
### VOTING RIGHTS RESTORATION

import selenium
from selenium import webdriver
from bs4 import BeautifulSoup
import csv

list1 = ["08", "10", "12", "14", "16", "18"]

for j in list1:

    with open("H:/Public/Democracy/Voting Rights & Elections/data/fec_donors/raw/old_data/weball" + j + "/ids" + j + ".csv", 'rb') as f:
        reader = csv.reader(f)
        your_list = list(reader)

    with open("H:/Public/Democracy/Voting Rights & Elections/data/fec_donors/temp/ids_unitem_" + j + ".csv", "wb") as t:
        writer = csv.writer(t)
        writer.writerow(["id", "total_receipts", "total_individ", "itemized_indiv", "unitemized_indiv"])
        path_to_chromedriver= "H:/Public/Democracy/Voting Rights & Elections/data/misc/chromedriver.exe"

        driver = webdriver.Chrome(executable_path = path_to_chromedriver)
        i = 1
        for item in your_list:
            try:
                if j in ["96", "98"]:
                    url = "https://www.fec.gov/data/candidate/" + "".join(item) + "/?cycle=19" + j
                else:
                    url = "https://www.fec.gov/data/candidate/" + "".join(item) + "/?cycle=20" + j
                driver.get(url)

                total_receipts = driver.find_element_by_xpath("//*[@id='total-raised']/figure/table/tbody/tr[1]/td[2]").text
                total_individ = driver.find_element_by_xpath("//*[@id='total-raised']/figure/table/tbody/tr[3]/td[2]").text
                itemized_indiv = driver.find_element_by_xpath("//*[@id='total-raised']/figure/table/tbody/tr[4]/td[2]").text
                unitemized_indiv = driver.find_element_by_xpath("//*[@id='total-raised']/figure/table/tbody/tr[5]/td[2]").text

                writer.writerow(["".join(item), total_receipts, total_individ, itemized_indiv, unitemized_indiv])
            except:
                print item



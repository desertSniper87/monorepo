from bs4 import BeautifulSoup as bsoup
from urllib.request import urlopen
from bs4 import BeautifulSoup

import sqlite3
from sqlite3 import Error

import requests as rq
import re
 
def create_connection(db_file):
    """ create a database connection to a SQLite database """
    try:
        conn = sqlite3.connect(db_file)
        print("Create table")
        create_table(conn)
        cur = conn.cursor()
        print(sqlite3.version)
        return conn
    except Error as e:
        print(e)
    # finally:
        # conn.close()

def create_table(conn):
    try:
        # conn.execute("""CREATE TABLE IF NOT EXISTS fertilizer_shops_fertilizer_shop(
                        # id INT PRIMARY KEY NOT NULL,
                        # name TEXT,
                        # village TEXT,
                        # union_council TEXT,
                        # upazila TEXT,
                        # district TEXT,
                        # division TEXT
                        # )""")
        conn.execute("""CREATE TABLE IF NOT EXISTS "fertilizer_shops_fertilizer_shop" ("village" varchar(30) NOT NULL, "union_council" varchar(30) NOT NULL, "upazila" varchar(30) NOT NULL, "district" varchar(30) NOT NULL, "division" varchar(30) NOT NULL, "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT, "name" varchar(30) NOT NULL);""")
    except Error as e:
        print(e)


if __name__ == '__main__':
    conn = create_connection("db.sqlite3")
    bfa_mem = "http://www.bfa-fertilizer.org/members-database/members.php?page="
    bfa_mem_page_url = 'http://www.bfa-fertilizer.org/members-database/'

    for i in range(1, 190):
        print("Page no. ", i)
        list_page_url = ''.join([bfa_mem, str(i)])
        page = urlopen(list_page_url)
        soup = BeautifulSoup(page, 'html.parser')
        x = soup.find_all('a', href= re.compile(r'.*memid.*'), target='_blank', class_='')
        members_url_list = [c["href"] for c in x]
        for member_url in members_url_list:
            id = member_url.split('=')[1]
            print("Collecting info for id ", id)
            member_page_url = ''.join([bfa_mem_page_url, member_url])
            member_page = urlopen(member_page_url)
            page_soup = BeautifulSoup(member_page, 'html.parser')
            page_soup_all_tr = page_soup.find_all('tr')

            for i in page_soup_all_tr:
                page_soup_all_td = i.find_all('td')
                for j in page_soup_all_td:
                    if j.string == 'Village : ':
                        village_td_list = []
                        for td in page_soup_all_td:
                            village_td_list.append(td.string)
                        village = village_td_list[3].strip()
                    elif j.string == 'Union : ':
                        union_td_list = []
                        for td in page_soup_all_td:
                            union_td_list.append(td.string)
                        union = str(union_td_list[3].strip())
                    elif j.string == 'Upazila : ':
                        upazila_td_list = []
                        for td in page_soup_all_td:
                            upazila_td_list.append(td.string)
                        upazila = upazila_td_list[3].strip()
                    elif j.string == 'District :':
                        district_td_list = []
                        for td in page_soup_all_td:
                            district_td_list.append(td.string)
                        district = district_td_list[3].strip()
                    elif j.string == 'Division : ':
                        division_td_list = []
                        for td in page_soup_all_td:
                            division_td_list.append(td.string)
                        division = division_td_list[3].strip()

            page_soup_all_p = page_soup.find_all('p')
            for j in page_soup_all_p:
                pattern = re.compile(r'Name of the Firm :.*')
                if pattern.match(str(j.string)):
                    name = j.string.split(':')[1]
            conn.execute("""INSERT INTO fertilizer_shops_fertilizer_shop 
                            (id, name, village, union_council, upazila, district, division)
                            VALUES (?, ?, ?, ?, ?, ?, ?)""", (id, name, village, union, upazila, district, division))

            conn.commit()
                    # for td in page_soup_all_td:
                        # name_td_list.append(td.string)
                # print(j)
                        # for 
            # print(page_soup_all_td)
            # for i in page_soup_all_td:
                # page_soup_all_tr_td = page_soup.find_all('td')
                # print(page_soup_all_tr_td)
                # print(i.string)

                # td_list = [c["td"] for c in page_soup_all_tr]
                # print(td_list)
            # print(page_soup)
        # x = soup.find_all('a', target= re.compile(r'_blank'))
        # x = soup.find_all('a', target='_blank')
        # print(x)
        # for i in x:
            # if (i.string!=None):
                # print(i.string.strip())
        # r = rq.get(list_page)
        # print(list_page)
        # print(soup.prettify())
        # print(soup.find_all(class_='table'))
        # print(soup.find_all(href=re.compile(".*memid.*")))
        # print(soup.find_all("a", href=re.compile(r".*details.php?memid=.*"), target='.*_blank.*'))
        # members_url_list = [c["href"] for c in soup.find_all("a", href=re.compile(r".*details.php?memid=.*"), target='.*_blank.*')]
        # members_url_list = [c["href"] for c in soup.find_all("a",  {'href': re.compile('.*details.php?memid=.*'), 'target': '_blank'})]
        # members_url_list = [c["href"] for c in soup.find_all("a", href=)]
        # print(members_url_list)

        # re.compile(r".*details.php?memid=.*") 
        # print(r)
        # print(i, bfa_mem.join(i))
        # print(rq.get(''.join([bfa_mem, str(i)])))
        # print()

    # with open("results.txt","wb") as acct:
        # for class_url in members_url_list:
            # base_url = "http://my.gwu.edu/mod/pws/{}".format(class_url)
            # r = rq.get(base_url)

            # soup = bsoup(r.text)
            # # Use regex to isolate only the links of the page numbers, the one you click on.
            # page_count_links = soup.find_all("a",href=re.compile(r".*javascript:goToPage.*"))
            # try:
                # num_pages = int(page_count_links[-1].get_text())
            # except IndexError:
                # num_pages = 1

            # # Add 1 because Python range.
            # url_list = ["{}&pageNum={}".format(base_url, str(page)) for page in range(1, num_pages + 1)]

            # # Open the text file. Use with to save self from grief.
            # for url_ in url_list:
                # print ("Processing {}...").format(url_)
                # r_new = rq.get(url_)
                # soup_new = bsoup(r_new.text)
                # for tr in soup_new.find_all('tr', align='center'):
                    # stack = []
                    # for td in tr.findAll('td'):
                        # stack.append(td.text.replace('\n', '').replace('\t', '').strip())
                    # acct.write(", ".join(stack) + '\n')

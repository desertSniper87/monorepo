import scrapy
from selenium import webdriver

# urls = [
    # 'https://www.hiiquote.com/quote/quote.php?Plan_ID=112&code=A157F6910039407D116147'
# ]

class LifeShieldStateSpider(scrapy.Spider):
    name = "lifeshield-states"

    def __init__(self):
        self.driver = webdriver.Chrome()

    def start_requests(self):
        urls = [
            'https://www.hiiquote.com/quote/quote.php?Plan_ID=112&code=A157F6910039407D116147'
        ]
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse, method='POST')

    def parse(self, response):
        # page = response.url.split("/")[-2]
        # filename = 'quotes-%s.html' % page
        # with open(filename, 'wb') as f:
            # f.write(response.body)
        # self.log('Saved file %s' % filename)
        self.driver.get(response.url)
        self.driver.find_element_by_xpath("""//*[@id="TabbedPanels1"]/div/div[1]/table/tbody/tr[1]/td/table/tbody/tr/td[2]/a/img""").click()
        # self.driver.find_element_by_tag_name('body').send_keys(Keys.CONTROL + Keys.TAB)
        # html = self.driver.find_element_by_id("State_Code").get_attribute('innerHTML')
        # print(html)
        print(self.driver.current_url)


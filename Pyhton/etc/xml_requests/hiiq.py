#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import requests

xml = """
<?xml version="1.0" encoding="iso-8859-1"?>
<NewBusiness>
  <Plan_ID>88</Plan_ID>
  <User_ID>A157F6910039407D116147</User_ID>
  <Mailing_City>ZZ11o7ai8</Mailing_City>
  <Effective_Date>2018-12-10</Effective_Date>
  <DOB>1994-11-28</DOB>
  <Feet/>
  <DayPhone>123-456-7890</DayPhone>
  <ESign_Send_Method>Email</ESign_Send_Method>
  <Quote_ID>86007469</Quote_ID>
  <Inch/>
  <IP_Address>127.0.0.1</IP_Address>
  <Email>torsho.92@gmail.com</Email>
  <CellPhone>123-456-7890</CellPhone>
  <Premium>2.56</Premium>
  <ESign_Option>Y</ESign_Option>
  <Mailing_Address>ZZ11o7pba</Mailing_Address>
  <EffectiveDate_Ack>Agree</EffectiveDate_Ack>
  <City>ZZ11o7ai8</City>
  <Age>24</Age>
  <Estate_Flag>1</Estate_Flag>
  <Occupation/>
  <ZipCode>44102</ZipCode>
  <Address>ZZ11o7pba</Address>
  <Mailing_State>OH</Mailing_State>
  <Name_Enroll>ZZ11lsj4e ZZ11v9p2g ZZ11ljgz9</Name_Enroll>
  <Name_Auth>ZZ11lsj4e ZZ11v9p2g ZZ11ljgz9</Name_Auth>
  <Payment_Agree>1</Payment_Agree>
  <Middle_Name>ZZ11v9p2g</Middle_Name>
  <Mailing_ZipCode>44102</Mailing_ZipCode>
  <Access_Token>201812090627175c0cfc15a975a</Access_Token>
  <Plan_Name>Option5000</Plan_Name>
  <Estate_Detail>ESTATE</Estate_Detail>
  <First_Name>ZZ11lsj4e</First_Name>
  <Esign_Option>Y</Esign_Option>
  <Weight>None</Weight>
  <SOC/>
  <Enrollment_Fee>0.00</Enrollment_Fee>
  <Last_Name>ZZ11ljgz9</Last_Name>
  <Gender>Male</Gender>
  <AdministrativeFee>5.00</AdministrativeFee>
  <State>OH</State>
  <Date_Signed>2018-12-09</Date_Signed>
  <Mailing_Name>ZZ11lsj4e ZZ11ljgz9</Mailing_Name>
  <CardHolder_Name>ZZ115dpgr</CardHolder_Name>
  <Card_Type/>
  <Billing_City>ZZ11o7ai8</Billing_City>
  <Billing_ZipCode>44102</Billing_ZipCode>
  <Card_ExpirationYear>2020</Card_ExpirationYear>
  <Payment_Method>BankDraft</Payment_Method>
  <Bank_Account_Name>ZZ110e8p8</Bank_Account_Name>
  <Bank_Account_Class>Checking</Bank_Account_Class>
  <Bank_Account_Type>Personal</Bank_Account_Type>
  <Card_ExpirationMonth>01</Card_ExpirationMonth>
  <Billing_Address>ZZ11o7pba</Billing_Address>
  <Billing_State>AL</Billing_State>
  <Payment_Agree>True</Payment_Agree>
  <Bank_Check_Number>100</Bank_Check_Number>
  <Bank_Name>ZZ11hx9so</Bank_Name>
  <Bank_Routing_Number>111000025</Bank_Routing_Number>
  <Card_Number/>
  <Bank_Account_Number>33463</Bank_Account_Number>
  <STMHealthQuestion/>
</NewBusiness>
"""
headers = {'Content-Type': 'application/xml'} # set what your server accepts
print(requests.post('http://www.hiiquote.com/webservice/process.php', data=xml, headers=headers).text)


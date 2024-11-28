#!/usr/bin/env python
# coding: utf-8

# In[ ]:


pip install requests


# In[1]:


import base64
import requests


# In[2]:


# Replace these with your actual API Key and Secret
api_key = 'vECg8empHmxAUV8TOmAG9TWxA6eA18Lv'
secret = 'jxXlEvraOK3fB2Vh'

# Base64 encode the credentials
credentials = f'{api_key}:{secret}'
base64_credentials = base64.b64encode(credentials.encode()).decode()

# Prepare the request headers and payload
headers = {
    'Authorization': f'Basic {base64_credentials}',
    'Content-Type': 'application/x-www-form-urlencoded'
}

data = {
    'grant_type': 'client_credentials'
}

# Make the request to the token endpoint
token_url = 'https://api.precisely.com/oauth/token'
response = requests.post(token_url, headers=headers, data=data)

# Check the response
if response.status_code == 200:
    token_info = response.json()
    access_token = token_info.get('access_token')
    print('Access Token:', access_token)
else:
    print('Error:', response.status_code, response.text)


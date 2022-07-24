#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd


# In[2]:


import numpy as np
import scipy
from scipy import stats


# # Load in Data Set

# In[4]:


avocados=pd.read_csv(r"C:\Users\Camille Kenworthy\Downloads\avocados\avocados.csv")


# In[5]:


avocados.head()


# # Data Wrangling

# ### Does the total volume of avocados sold differ between Indianapolis, Orlando, and PhoenixTucson? 

# In[9]:


avocados['Date'] = pd.to_datetime(avocados['Date'])
avocados.head()


# In[10]:


import seaborn as sns
sns.set_style('white')


# ## Let's plot each region's average price & color by the year of the observation

# In[12]:


mask = avocados['type']=='conventional'
g = sns.factorplot('AveragePrice','region',data=avocados[mask],
                   hue='year',
                   size=8,
                   aspect=0.6,
                   palette='Blues',
                   join=False,)


# In[13]:


order = (
    avocados[mask & (avocados['year']==2018)]
    .groupby('region')['AveragePrice']
    .mean()
    .sort_values()
    .index
)


# In[15]:


g = sns.factorplot('AveragePrice','region',data=avocados[mask],
                   hue='year',
                   size=8,
                   aspect=0.6,
                   palette='Blues',
                   order=order,
                   join=False,)


# ## Let's look more closely at how prices in these two cities fluctuate over the year. First, we'll filter down to only these two regions.

# In[16]:


regions = ['PhoenixTucson', 'Indianapolis']


# In[17]:


mask = (
    avocados['region'].isin(regions)
    & (avocados['type']=='conventional')
)


# In[18]:


avocados['Month'] = avocados['Date'].dt.month
avocados[mask].head()


# In[19]:


g = sns.factorplot('Month','AveragePrice',data=avocados[mask],
               hue='year',
               row='region',
               aspect=2,
               palette='Blues',
              )


# In[20]:


regions = ['Orlando', 'Indianapolis']


# In[21]:


mask = (
    avocados['region'].isin(regions)
    & (avocados['type']=='conventional')
)


# In[22]:


avocados['Month'] = avocados['Date'].dt.month
avocados[mask].head()


# In[25]:


g = sns.factorplot('Month','AveragePrice',data=avocados[mask],
               hue='year',
               row='region',
               aspect=2,
               palette='Blues',
              )


# In[ ]:





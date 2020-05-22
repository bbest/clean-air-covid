# ucsb-meds-teaching-demo
Teaching demonstration for UCSB Masters in Environmental Data Science

## Setup

### Apply for accounts

#### Google Earth Engine (GEE) account

Please visit:

- [Google Earth Engine signup](https://signup.earthengine.google.com/#!/)

You may need to log out and back into your web browser as the preferred Google account to request permissions. This approval process can take days to weeks unfortunately.

#### Twitter developer account

Please visit:

- [Twitter developer signup](https://developer.twitter.com/en/apply-for-access)

### Install R packages

```r
# use librarian to load libraries, installing if needed
if (!require("librarian"))
  install.packages("librarian")
library("librarian")

shelf("r-spatial/rgee")
```


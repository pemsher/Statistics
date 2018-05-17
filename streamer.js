
const WebSocket = require('ws');
// Utility
    function jsonToQueryString(json) {
        return Object.keys(json).map(function(key) {
                return encodeURIComponent(key) + '=' +
                    encodeURIComponent(json[key]);
            }).join('&');
    }

var userPrincipalsResponse = {
  "userId": "pemsher022966",
  "userCdDomainId": "A000000034111383",
  "primaryAccountId": "868477655",
  "lastLoginTime": "2018-04-18T19:42:24+0000",
  "tokenExpirationTime": "2018-04-19T02:51:13+0000",
  "loginTime": "2018-04-19T02:21:13+0000",
  "accessLevel": "CUS",
  "stalePassword": false,
  "streamerInfo": {
    "streamerBinaryUrl": "streamer-bin.tdameritrade.com",
    "streamerSocketUrl": "streamer-ws.tdameritrade.com",
    "token": "38ecc81f454044231813690238d049036a37de14",
    "tokenTimestamp": "2018-04-19T02:26:51+0000",
    "userGroup": "ACCT",
    "accessLevel": "ACCT",
    "acl": "BPDRDTESF7G1G3G5G7GKH1H3H5M1MAOSPNQSRFSDSPTBTETFTOTTUAURXBXNXOQ2NSD2D4D6D8E2E4E6E8F2F4F6H8I2",
    "appId": "DP"
  },
  "professionalStatus": "NON_PROFESSIONAL",
  "quotes": {
    "isNyseDelayed": false,
    "isNasdaqDelayed": false,
    "isOpraDelayed": false,
    "isAmexDelayed": false,
    "isCmeDelayed": true,
    "isIceDelayed": true,
    "isForexDelayed": true
  },
  "streamerSubscriptionKeys": {
    "keys": [
      {
        "key": "ac4129ef53cafb4d710f123c1b4d208932a5ca54f87f29443f68e6199d5eeb3c8"
      }
    ]
  },
  "accounts": [
    {
      "accountId": "868477655",
      "displayName": "pemsher022966",
      "accountCdDomainId": "A000000034111384",
      "company": "AMER",
      "segment": "ADVNCED",
      "acl": "BPDRDTESF7G1G3G5G7GKH1H3H5M1MAOSPNQSRFSDSPTBTETFTOTTUAURXBXNXOQ2NS",
      "authorizations": {
        "apex": true,
        "levelTwoQuotes": true,
        "stockTrading": true,
        "marginTrading": true,
        "streamingNews": true,
        "optionTradingLevel": "SPREAD",
        "streamerAccess": true,
        "advancedMargin": true,
        "scottradeAccount": false
      }
    }
  ]
}

    //Converts ISO-8601 response in snapshot to ms since epoch accepted by Streamer
    var tokenTimeStampAsDateObj = new Date(userPrincipalsResponse.streamerInfo.tokenTimestamp);
    var tokenTimeStampAsMs = tokenTimeStampAsDateObj.getTime();

var credentials = {
    "userid": userPrincipalsResponse.accounts[0].accountId,
    "token": userPrincipalsResponse.streamerInfo.token,
    "company": userPrincipalsResponse.accounts[0].company,
    "segment": userPrincipalsResponse.accounts[0].segment,
    "cddomain": userPrincipalsResponse.accounts[0].accountCdDomainId,
    "usergroup": userPrincipalsResponse.streamerInfo.userGroup,
    "accesslevel": userPrincipalsResponse.streamerInfo.accessLevel,
    "authorized": "Y",
    "timestamp": tokenTimeStampAsMs,
    "appid": userPrincipalsResponse.streamerInfo.appId,
    "acl": userPrincipalsResponse.streamerInfo.acl
}

var request = {
    "requests": [
            {
                "service": "ADMIN",
                "command": "LOGIN",
                "requestid": 0,
                "account": userPrincipalsResponse.accounts[0].accountId,
                "source": userPrincipalsResponse.streamerInfo.appId,
                "parameters": {
                    "credential": jsonToQueryString(credentials),
                    "token": userPrincipalsResponse.streamerInfo.token,
                    "version": "1.0"
                }
            }
    ]
}

var request1 = {
    "requests": [
        {
            "service": "OPTION",
            "requestid": "2",
            "command": "SUBS",
            "account": userPrincipalsResponse.accounts[0].accountId,
            "source": userPrincipalsResponse.streamerInfo.appId,
            "parameters": {
                "keys": "AAPL_042018P172.5",
                "fields": "0,1,2,3,4,5,6,7,8"
            }
        }
    ]
}

console.log(userPrincipalsResponse.streamerInfo.streamerSocketUrl );

var mySock = new WebSocket("wss://" + userPrincipalsResponse.streamerInfo.streamerSocketUrl + "/ws"); 

mySock.onmessage = function(evt) { console.log(evt.data); };
mySock.onclose = function() { console.log("CLOSED"); };
mySock.onopen = function (evt) {
   mySock.send(JSON.stringify(request));
   mySock.send(JSON.stringify(request1));
};

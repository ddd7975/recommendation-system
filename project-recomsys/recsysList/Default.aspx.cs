using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using Microsoft.WindowsAzure.Storage;
using Microsoft.WindowsAzure.Storage.Auth;
using Microsoft.WindowsAzure.Storage.Table;
using System.Configuration;

namespace recsysList
{
    public partial class Default : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            
            AddCookie("1-2JKD1I");
            AddCookie("1-2JKBZE");
            AddCookie("1-2JKCUD");
            AddCookie("1-2JKD0B");
            
            AddCookie("2C743FDB-0E91-496D-9B86-92B0075B292D");
            AddCookie("1-2JKD0B");
            AddCookie("1-2JKCWN");
            AddCookie("870BB506-D576-43F0-BFAD-BC6533086ACB");
            
            /*
            AddCookie("1-2JK");
            AddCookie("1-2JK");
            AddCookie("870BB506");
            */
            //read the updated cookie
            HttpCookie myCookie = Request.Cookies["myTestCookie"];

            // default name is recommander1, can be changed in Default.aspx
            recommander1.firstProduct = myCookie.Values["1"];
            recommander1.secondProduct = myCookie.Values["2"];
            //recommander1.userID = "00027AEA-D94F-414C-8601-DB31D4D22D19";
            recommander1.userID = "00027AEA-D94F-414C";
            recommander1.blank = "blank";
        }

        // cookie更新function
        public string AddCookie(string state)
        {
            //獲取客戶端的cookie
            HttpCookie myCookie = Request.Cookies["myTestCookie"];
            if (myCookie == null)
            {
                //Gernerate a Cookie 產生一個cookie名為myCookieNew
                HttpCookie myCookieNew = new HttpCookie("myTestCookie");
                //set value 設定Key值以及值
                myCookieNew.Values.Add("1", "BLANK");
                myCookieNew.Values.Add("2", state);
                //set expired date
                myCookieNew.Expires = DateTime.Now.AddDays(2);
                //write the result 將結果寫入
                Response.AppendCookie(myCookieNew);
            }
            else
            {
                // 若已有狀態，則加入新的狀態
                myCookie.Values["1"] = myCookie.Values["2"];
                myCookie.Values["2"] = state;
                // 將結果寫入
                Response.AppendCookie(myCookie);
            }
            return "";
        }

    }
}
<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Default.aspx.cs" Inherits="recsysList.Default" %>

<%@ Register src="recommander.ascx" tagname="recommander" tagprefix="uc1" %>

<%@ Register src="WebUserControl1.ascx" tagname="WebUserControl1" tagprefix="uc2" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
</head>
<body>
    <form id="form1" runat="server">
    <div>    
    </div>
        <p>
            &nbsp;</p>
        <p>
        <uc1:recommander ID="recommander1" runat="server" />
        </p>
    </form>
</body>
</html>

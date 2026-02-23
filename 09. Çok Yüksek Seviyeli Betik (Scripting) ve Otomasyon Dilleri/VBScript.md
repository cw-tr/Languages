# VBScript (Visual Basic Script)

## Özet
VBScript (Visual Basic Scripting Edition); 1996 yılında Microsoft tarafından klasik Visual Basic (VB) dilinin zayıflatılmış ve tarayıcı/sistem otomasyonu için hafifletilmiş versiyonu olarak icat edilen, 2000'li yıllarda Windows çökmelerinin ("I Love You" virüsü vb.) baş aktörü olan ve **Sadece İnternet Explorer veya Windows İşletim Sistemleri içinde (WSH)** çalışabilen ölü/tarihi bir betik dilidir.

## Nedir ve Ne İşe Yarar?
1990'ların sonunda Netscape firması İnternet Tarayıcısına "Canlılık" katmak için **JavaScript**'i icat ettiğinde, Microsoft hemen panikleyip "Bizim de tarayıcımız İnternet Explorer (IE)'nin diline aynı şeyi entegre etmemiz lazım" dedi. Açık kaynak standart almak yerine, 1995'te dünyayı domine ettikleri klasik "RAD/Visual Basic" dillerinin kodunu `VBScript` adıyla tarayıcının kalbine gömdüler.

JavaScript'in C tarzı `{}` süslü sözdizimine karşı; VBScript düz İngilizce kelimelerden (`If...End If`) oluşan BASIC dilini internete getirdi. İkinci büyük rolü ise, DOS/Cmd bat dosyalarının iğrençliğinden bıkan Windows Sistem Adminleri (SysAdmins) için **"Windows Script Host (WSH)"** eklentisiyle C diskinde dosya silecek, format atacak otomasyon dilleri (`.vbs` uzantısı) olarak masaüstünde çılgınca varlık göstermesidir.

**Ne İşe Yarar?**
* **1990'ların ASP(Eski Klasik) Web Mimarisi:** PHP veya C# gelmeden önce; Microsoft sunucularında Web sitesi yaratan (Veritabanından tablo çeken) orijinal **Classic ASP** (Active Server Pages) kodları baştan ayağa %100 VBScript ile yazılırdı `<% ... %>` tagları arasında!
* **Windows'un Meşhur Zararlıları (Malware):** Windows `.vbs` uzantılı bir dosyanın içine tıklandığında, Kod gidip (FileSystemObject aracılığıyla) Outlook Maillerini açma ve klasör silme yeteneğine sahipti. Tarihin efsanevi bilgisayar çökerten ("Seni Seviyorum" - I Love you Virüsü) tamamen VBScript ile kodlanmış bir şakadır. 

## Dilin Mantığı ve Kod Yapısı
Tam bir **Visual Basic 6 (VB6) Klonudur**, fakat hafifletilmişidir. Tipler Ciddi Değildir (Strict Typing Yoktur). Integer, String, Array diye bir şey deklare edemezsiniz; bütün tipler meşhur **`Variant` (Ne gelirse O olur)** adlı hayalet bir tip ile tanımlanmak zorundadır.

Windows `COM (Component Object Model)` Nesnelerine doğrudan bağlanır. "MsgBox" dediğiniz an Windows'un kalbinden grafik API koparır ekrana Dialog uyarı basar. Okunması Javascript'ten (Liselere/amatörlere göre) çok daha rahattır lakin çok büyük Microsoft bağımlığı içerir.

**Örnek İşleyiş (Sembolik Olarak):**
Tarayıcıda JavaScript: `alert("Selam");`
Tarayıcıda/Windowsda VBScript: `MsgBox "Selam"` (Düz metin gibi harfi basar).

### Örnek Bir VBScript (.vbs) Kodu: Windows Sistemine Hükmetmek (FileSystemObject)
İnternet Explorer döneminden kopup gelen (Veya masaüstünde çift tıklandığında çalışan); Windowsun C diski çekirdeğine sızıp Dosya Yaratan ve Ekrana Pop-up patlatan tipik otomasyon Script'i:

```vbscript
' VBScript(Basic serisi) yorumlari Kesme/Tırnak (') işareti ile baslar!

' 1. DEĞİŞKEN TANIMLAMA (Her Sey VARIANT Tipindedir!)
' VBScriptte Tipler 'Integer/String' YOKTUR. 'Dim' ile degisken boyutlandirilir
Dim fsoBilgisayarSistemi     ' Dosya sistemine ulasacak obje
Dim sistemRaporDosyasi       ' Text Dosyasini tutacak Obje
Dim islemSonucu              ' Dialog ekranini tutacak degisken

' 2. WINDOWS COM NESNESİ (Wscript / CreateObject) ÇAĞIRISI! MUCİZE KAPI:
' CreateObject ile Windows'un derin kütüphanesi "Scripting.FileSystemObject" Motorunu cekip DEGIİSKENE ata!
' DIKKAT: Objeler atnarken C#'daki gibi(=) degil, VBScriptte ozel 'Set' Kelimesi kullanilir:
Set fsoBilgisayarSistemi = CreateObject("Scripting.FileSystemObject")


' 3. SİSTEMİ ÇALIŞTIRMA (C diskine Dosya Bas):
' Belirtilen yola Text (Txt) dosya yarat (True = Eger onceki varsa Üstune Yazma Vurus Yap):
Set sistemRaporDosyasi = fsoBilgisayarSistemi.CreateTextFile("C:\Logs\WindowsVBScript_Test.txt", True)

' Dosyanın icini yaz!
sistemRaporDosyasi.WriteLine("VBScript Tarafindan Otomatik Bir Islemdir.")
sistemRaporDosyasi.WriteLine("Bu dil Javascriptin kani olarak Microsoft tarafindan Internet Explorerda dayatilmistir!")

' Dosyayi kapat ki Windows Bellekten(RAM'den) dusursun
sistemRaporDosyasi.Close


' 4. IF / DONGUSU VE ŞIK EKRAN BASKA (MSGBOX)
' VBScriptin C, C++ ve Javadaki O Igrenc Suslu Parantezleri ({}) cöpe atan o 'Then / End IF' Cümlesi:

If fsoBilgisayarSistemi.FileExists("C:\Logs\WindowsVBScript_Test.txt") Then
    
    ' EGER Dosya Cidden var olduysa! MSGBOX (Pop-Up) ile Windows Ekranina Hata/Bilgi patlat:
    ' 64 Rakamı: Ekranda I(Info/Bilgi) Ikonlu Windows Mesaji cikarir.
    MsgBox "Tebrikler Coder! Windows Logs basariyla sızıldı ve dosya yazildi!", 64, "Sistem Basarili!"
    
Else
    
    MsgBox "Eyvah, Guvenlik Dosya yaratilmasina izin vermedi!", 16, "Kritik Hata"

End If ' Donguyu Sonlandir!
```
Bu dosya uzantısını `Test.vbs` yapıp masaüstünde tıkladığınızda; Arkaplanda Siyah Terminal Asla gözükmez, bilgisayar anında bir saniyede Text dosyasını açar ve sizin karşınıza renkli/sesli "Sistem Başarılı!" MsgBox kutusu (Dınk!) sesiyle çıkartır. Bu yüzden Virüs yazarlarının efsanevi gözdesi oldu.

## Kimler Kullanır?
* Klasik **Eski Kurumsal Microsoft Sistem (Aktif Dizin/Active Directory) Adminleri**, VBScript ile personellerin sabah bilgisayarlarını açtıklarında "Ağa Otomatik bağlanma" Login scriptleri yazarlardı. 
* İnternet aleminde "Classic ASP" sitelerini yaşatmaya çalışan eski topraklar.
* **GÜNÜMÜZDE ÖLÜDÜR.** Microsoft 2023 yılında VBScript'i resmen "Terk Ettiğini (Deprecated)" ve Windows Update ile sistemden sileceğini açıkladı. Yerini tamamen **PowerShell**'e ve tarayıcı piyasasını ebediyen **JavaScript**'e kaptırmış, bir çağın (Milenyum/Virüs döneminin) kapı aralığında tarihe gömülmüştür.

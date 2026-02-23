# Visual Basic

## Özet
Visual Basic (VB / VB.NET); 1991 yılında Microsoft (Alan Cooper ve ekibi) tarafından icat edilen, yazılım tarihinin bilgisayar ekranında "Fare ile Sürükle-Bırak (Drag-and-Drop)" yöntemini Windows ile bütünleştirerek programlama dünyasına GUI(Arayüz) devrimini yaşatan, İngilizceye aşırı benzeyen sözdizimi ile yazılımı **"halkın ve amatörlerin de yazabileceği"** muazzam erişilebilirliğe taşıyan efsanevi dildir.

## Nedir ve Ne İşe Yarar?
1990'ların ilk yarısında Windows'a bir masaüstü uygulaması (Örn: Hesap Makinesi) yazmak için C/C++ dillerinde karmaşık "Win32 API" kodları ve bellek Pointerları yazmak, bir kutuyu ekranda 5 piksel sağa kaydırmak için sayfalar dolusu koordinat girmek gerekiyordu. 

Microsoft bu zorluğu "Görsel (Visual)" kelimesiyle parçaladı. Sol tarafa bir Araç Kutusu (Toolbox) koydular. O araç kutusundan ekrana bir "Button" tutup sürüklediniz, üstüne çift tıkladınız ve ekranın arkasına Türkçe/İngilizce kadar basit olan BASIC dilinde `MsgBox "Merhaba"` yazdınız. Derle dediniz, programınız çalıştı! Delphi gelene kadar (Delphi ondan hızlıdır ama VB çok daha anlaşılırdı) VB bu gezegendeki masaüstü yazılım krallığını tek başına kurdu.

**Ne İşe Yarar?**
* **Hızlı Karalama / Prototipleme (RAD):** Şirketlerin iç muhasebe otomasyonları, depo kayıt ekranları veya anlık şifreleme/çözme panelleri gibi "Çok hızlı ekrana çıkmalı" dediği 2000'lerin başlarındaki bütün ticari B2B Windows projeleri (Özellikle VB6 ile) yazıldı.
* **Microsoft .NET Ekosistemi (VB.NET):** 2002'lerden sonra orijinal VB (VB6) öldürüldü ve yerine C#'ın alt motorunu birebir kullanan ama İngilizce yazım kuralına (`{}` yerine `End Sub` barındıran) devam eden devasa Güçlü (OOP) **Visual Basic .NET** çıkarıldı.

## Dilin Mantığı ve Kod Yapısı
Evrendeki en okunabilir (Neredeyse düz metin/İngilizce) kurguya sahiptir. Süslü parantezler `{ }` VEYA noktalı virgüller (`;`) YOKTUR. Onların yerine İngilizce bitiş kelimeleri (`End If`, `End Sub`, `Next`) kullanılır.

Harf büyüklüğüne veya küçüklüğüne (Case-Sensitivity) DİKKAT ETMEZ. Siz `mesaj` isimli bir değişken yaratıp aşağıda `MESAJ = 5` yazarsanız C dili çökerken, VB bunu anında tolere edip IDE'nin içinde (Visual Studio) ikisinin de harflerini kendisi otomatik düzenler ve çalışır. Yazılımcıyı şımartan ve işkenceden kurtaran bir dosttur.

**Örnek İşleyiş (Sembolik Olarak):**
C# dilindeki sert ve köşeli if döngüsü: `if (sayi == 5) { Console.WriteLine("Evet"); }`
Visual Basic'teki edebi kurgu: `If sayi = 5 Then Console.WriteLine("Evet") End If` (Adeta "Eğer sayı 5 ise O halde... ve Son!" diyerek masal okur gibi kodlatır).

### Örnek Bir Visual Basic (VB.NET) Kodu: Windows Form Ekranına Tıklamak
O efsanevi Sürükle-Bırak kültürünün, Visual Studio üzerinden üzerine çift tıklandığında açtığı "Olay Tetikleyicisi (Event Handler)" arkasında çalışan o İngilizce-Vari (.NET) motoru:

```vbnet
' Visual Basic dilinde Yorum Satirlari Tırnak (Apostrophe - ') isareti ile baslar!

' .NET Kutuphaneleri Iceri Alinir (Imports)
Imports System.Windows.Forms

' Arka planda formun kendisi bir Sinif (Class) tır, biz gormesek bile VB.NET %100 OOP'dir
Public Class AnaPencere

    ' Biz Windows Ekranina (Forma) Form-Tasarlayıcı araciligiyla buton koyariz 
    ' Cift Tikladigimizda Bizim Adimiza BU KOD BLOGU Uretilir (Subroutine - Sub)
    Private Sub TiklaButonu_Click(sender As Object, e As EventArgs) Handles TiklaButonu.Click
        
        ' KURAL: Dimension(Boyut/Degisken). 'Dim' kelimesiyle degisken yaratilir.
        ' Noktalı virgül (;) ve Süslü Parantez ({) ASLA kullanilmaz!
        Dim kullaniciAdi As String = "Kral Geliştirici"
        Dim hesapla As Integer
        
        hesapla = 100 * 50

        ' Klasik VB: If Dongusu (Then / Else / End If kelimeleri pürüzsüzlügü)
        If hesapla > 1000 Then
            
            ' MessageBox (MsgBox), C dilinde binlerce satir suren grafik API cizimini 
            ' Microsoft'un bu işi tek kelimeye hapsetmesidir: 
            MessageBox.Show("Sistem Basariyla Kuruldu! " & kullaniciAdi, "Bilgi Ekrani")
            
        Else
            
            MessageBox.Show("Hesaplama Hatali.", "Hata Ekrani")
            
        End If ' Döngüyü Edebi olarak Bitir

    End Sub ' Prosedüru Bitir

End Class
```
Siz sadece `"Play"` tuşuna basardınız. Visual Studio saniyeler içinde bütün arka plan Windows Grafik (WinForms) kütüphanelerini C++ çekirdeğine derleyip size .EXE (çalıştırılabilir dosya) olarak verirdi. Programlamayı elitlerin tekelinden alıp Liseli gençlerin/Hobici kitlelerin hilesine sunan yapıdır.

## Kimler Kullanır?
* 2000'li yıllarda piyasaya giren ama "C dillerinin o karmaşık Pointer/Memory" yapısından korkan on milyonlarca Alaylı yazılımcı ve Kurumsal İçi IT Görevlileri.
* Microsoft Office programlarının (Excel, Word) içine kendi otomasyon makrolarını yazan Finansçılar (VB'nin kardeşi olan **VBA - Visual Basic for Applications** vasıtasıyla hala Excel denince dünyayı yöneten dildir).
* Günümüzde Microsoft artık VB.NET'e yeni özellikler eklemeyeceğini duyurduğu için **Sadece C# yazamayan eski toprak Legacy bakımı yapan geliştiriciler** tarafından kullanılmaktadır. Herkes zorunlu olarak C#'a(Kardeşine) geçmiştir.

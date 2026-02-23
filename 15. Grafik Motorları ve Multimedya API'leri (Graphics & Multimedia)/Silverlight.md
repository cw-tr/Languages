# Silverlight

## Özet
Microsoft Silverlight; 2007 yılında Microsoft firması tarafından, Adobe Flash'in (Bir üstteki belgedeki) Küresel Hükümdarlığına/Tekeline **"Rakip Olmak, Flash'i Kendi Zemininde Ezerek İnternetin Yeni Zengin(Zengin İçerikli-Önyüz / RIA) Medya Eklentisi Oymak"** amacıyla inanılmaz Bir Bütçeyle Piyasaya Sürülen; Arka planda .NET Dillerini (C# / VB) Ve **XAML** Tasarım Yapısını koşturan, Fakat Tıpkı Flash Gibi HTML5 Dalgalarında Boğularak 2021(ve 2012 lerde)'lerde Çöpe Atılan/Sonlandırılan Devasa Bir Teknolojik Hayal Kırıklığıdır.

## Nedir ve Ne İşe Yarar?
2007 Yılında Microsoft (Dünyanın Yazılım Deviydi) Ancak "İnternet Tarayıcısı Ortamında" (YouTube videoları, Web Oyunları Oynatmakta) İp Adobe'un(Flash'ın) elindeydi. Geliştiriciler Flash'ın ActionScript dilini ve Arayüzünü Seviyordu. Microsoft PC dünyasındaki C# Dominasyonunu WEB'e Çekmek İstedi.

"Silverlight Eklentisini(GümüşIşık)" çıkardı. İnternete giren Müşterilere (Flash gibi) 5-10 Megabytel'ik bir ".EXE / Eklenti" indirtti ki InternetExplorer(IE) veya Firefox Üzerinde Silverlight Videoları, Menüleri Mükkemel Grafiklerle, HD(yüksek çözünülürkük) Videolarla Oynasın.

**Ne İşe Yarar? (Zamanındaki Umutları):**
* **Zengin İnternet Uygulamaları (RIA - Rich Internet Applications):** Browserda Excel klonları yazmak, Borsacıların Bilgisayara İndirmeden (Tarayıcıda) pürüzsüz Takip Çizgilerini İzlediği Banka Menüleri Yaratmak.
* **Premium Medya Stream (Netflix Yayıncılığı):** Microsoft, Silverlight'ı özellikle DRM (Kopyalama koruması) Ve Yüksek Çözünürlüklü Yayıncılıkla güçlendirdi. Hatta **NETFLIX**, Uzun bir Dönem Boyunca Kullanıcılarına Film İzletebilmek İçin Tüm Web Altyapısında MAC ve PC'lerde "SILVERLIGHT" Çerçevesini ve Playerını Kullanarak Dünyaya Yayın yaptı!(En büyük başarısıydı). 

## Dilin Mantığı ve Kod Yapısı
Microsoft'un Windows Masaüstünde (WPF Teknolojisi - Windows Presentation Foundation) Ne varsa, Alıp %100 Birebir Tarayıcı içine Gömülmüş C# Mimarısıydi.

1. **Önyüz Tasarımı Çizmek (Amelelik-XAML):** Tasarımlar XML dilinin modifiyesi olan **XAML (eXtensible Application Markup Language)** İle Tıpa Tıp Android Studio kodlar gibi Şişik kodlarla (Buton Width vs) yazılırdı. 
2. **ArkaPlan Mantığı ve Düğme Davranışı (Code-Behind):** XAML daki Butona Basınca Çalışacak Kodlar Safkan **C#** Veya **Visual Basic .NET** Diliyle Daktilo Edilrir (Arka Planda Exeye/DLL) Compile Edilirdi). 
3. Dosyalar .SWF yerine **`.XAP`** Formatında Ziplenip Müşteriye Fırlatılırdı.

**Örnek İşleyiş:** Çok kurumsal , C# Alışkın Abilerin (Masaüstü yazılımcılarının) Şok geçirmeden 2 Günde Web Sitesi Çizebimesidir.

### Örnek Bir Silverlight Mimarisi (Masaüstü .NET Kodunun Tarayıcıya Zerk Edilişi)
Aşağıda Hem Görüntüyü Süsleyen C#, hemde Butona Tıklayan O Korkunç derecede "Masaüstümsü" Web Programı:

```xml
<!-- 1. ADIM: (Page.xaml DOSYASI) - WEB SITESININ (FLASH MUADILI) GORSELLIGININ OZELLIKLERI: -->
<!-- Butonu Tanimla. İsmi Ver (x:name) Ve Tiklaninca C# taki Olayi Tetikle(Click Mühürü): -->

<UserControl x:Class="SilverlightUygulamam.AnaSayfa"
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    Width="400" Height="300">
    
    <!-- Esnek Kutu (Grid) Olustur Cizime Basla -->
    <Grid x:Name="LayoutRoot" Background="LightBlue">
        
        <!-- Yazi ve Buton Yerleşimleri: -->
        <TextBlock x:Name="SonucYazisi" Text="Bekleniyor..." 
                   FontSize="24" HorizontalAlignment="Center" VerticalAlignment="Top" Margin="20"/>

        <!-- İsTe Silverlightin GucU: Click Olayina (C#) Metodnu Bagla! -->
        <Button x:Name="btnTiklaMucizesi" Content="Microsoft Gucunu Goster!" 
                Width="200" Height="50" HorizontalAlignment="Center" VerticalAlignment="Center" 
                Click="btnTiklaMucizesi_Click" />

    </Grid>

</UserControl>
```

```csharp
/* 2. ADIM: (Page.xaml.cs GOZÜKMEZ/ARKA PLAN KOD DOSYASI) */
// Tamamen Saf C# Dir. İcinde LINQ, Async/Await her turlu C# Fantezisi Tarayicida calisir!

using System.Windows;
using System.Windows.Controls;

namespace SilverlightUygulamam 
{
    // Arayuzdeki AnaSayfa sinifini Temsil Eden Yarı(Partial) Class Mimari!
    public partial class AnaSayfa : UserControl 
    {
        // KURUCU METHOD (Sayfa TArrayicida Renderlanirken!)
        public AnaSayfa() 
        {
            InitializeComponent(); // XAML Bilesenlerini C#'a Baglar
        }

        // TIKLANMA OLAYININ TETIKLENMESI(Event Handler) 
        // Kullanıci yukaridaki Butona Web Sayfasinda Bastigi Anda Burasi Isler:
        private void btnTiklaMucizesi_Click(object sender, RoutedEventArgs e) 
        {
            // İsterseniz Tarayiciya Windows(MasaUstu Gibi Hata Uyari Ekrani firlat):
            MessageBox.Show("Boom! Silverlight ile Tarayicinin İçine C# Soktum!");
            
            // Yada HTML (Dom daki) Yaziyi Degisitir. (JSX'e Gerek kalmadan C# gucu:
            SonucYazisi.Text = "Kuralı .NET Dili Bozar...";
        }
    }
}
```
Programcılar "Vay Canına Taraycıda C# .NET Yazıyoruz, HTML ve PHP eziyetlerine Gerek Kalmadı!" diye Bayram Ettiler! Lakin... 

## Neden Öldü? Milyar Dolarlık İnadın Çöküşü 
1. Microsoft; Dünyadaki Milyarlarca Kullanıcısını *"Şu Silverlight Player'ı lütfen Bilgisayarına İndir de Sistemlerin Çalışsın Lütfen"* diye İkna Edemedi! Herkes Zaten Flash Kurmuştu, 2. bir Zımpırtıya Gerek Yoktu (Pazarın Yüzde 70'i Umursamadı).
2. Apple (iPhone)'da ASLA Çalışmıyordu ve Mobil Piyasada Sıfır Çekti.
3. Flash ile Aynı Anda (HTML5 Çıkartması ve `<video>` Canavarıyla) Render yeteneklerinin Javascript Tarafından(Vuejs/React Tarafından) ele Geçirilemsiyle Son Buldu. Netflix Biile Onu Terkettiğinde Fişi Çekilip Gömüldü.

**Rönesansı (Günümüz Yeniden Doğumu - BLAZOR):** Microsoft pes Etmedi. Günümüzde (WebAssembly Teknolojsi icat Edilince) Yıllar Sonra "Abi Eklenti İndirmeden Chrome'da C# Çalıştırıyoruz" DIYEREK **Blazor (WebAssembly)** Teknolojsini Cıkartti Ve Su Anda Silverlightin Ruhunu Muhteşem Modern Bir Yapıyla (Yeniden C#'ı Webe Sızdırarak) canlandırttı. Silverlight Ölse de Ruhu Yaşamaya Yeniden Microsoft ile Dirildi.

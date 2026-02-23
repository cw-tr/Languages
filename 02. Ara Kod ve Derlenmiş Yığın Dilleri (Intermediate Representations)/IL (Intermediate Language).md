# IL (Intermediate Language) / MSIL

## Özet
IL (Intermediate Language / Ara Dil) ya da Microsoft dünyasındaki tam adıyla **MSIL / CIL (Common Intermediate Language)**; 2000'lerin başında Microsoft'un .NET platformunu ("Tüm Windows dillerini birbirine bağlayacağız!" diyerek) yaratırken icat ettiği, Tıpkı Bytecode'a benzeyen ancak çok daha **Nesne-Yönelimli (Object-Oriented) ve Zengin** Meta-Verilerle tasarlanmış Kapsamlı (ve standartlı) Ara Makine Kodudur. C#, F# veya VB.NET dillerinden hangisini kullanırsanız kullanın, Hepsi aynı Kırmızı huniye(IL'ye) dökülerek Eşitlenir!

## Nedir ve Ne İşe Yarar?
Eskiden Microsoft firmasında "Visual Basic" kullanan bir yazılımcı İle "C++" kullanan yazılımcı aynı kütüphaneyi veya Fonksiyonu Paylaşamazdı. Çünkü Diller Farklı exe'lere Cıkıyordu.

Microsoft "Common Language Runtime (CLR - Ortak Dil Çalışma Zamanı)" dediği Efsanevi .NET motorunu İnşa etti. Ve dedi ki: **Bundan Sonra C# da Yazsanız, F# da yazsanız... İkinizin Kodu da Makine (Assembly/Exe) Ye değil, IL (Ara Dil) KODUNA GEÇECEK. Çıkan DLL ve EXE Dosyalarının İçinde İNTEL veya AMD kodu YOKTUR! İçerisinde IL Kodu Vardır!**
O Çıkan program Müşterinin (Kullanıcının) Bilgisayarına Atılıp Çift Tıklandığı Saniye (JIT - Just in Time Compiler Tarafından), O bilgisayarın Gerçek RAM/İşlemcisine uygun "ÖZEL SAF DİLE (Native)" Işık hızında çevrilir ve Çiçek gibi Başlar.

**Ne İşe Yarar?**
1995'te Java Devrimi Oldu. Java dedi ki: **Sizin Makinenize (Windows/Linux) göre EXE üretmeyeceğim! Ben (Bytecode) adında Kendi Sanal Bilgisayarımın dilini üreteceğim.**
Siz Java (veya Python) kodu yazdığınızda Arka planda bir `Bytecode` Üretilir. Bu bytecode Cihaza Bağlı DEĞİLDİR! O bytecode'u Mac'e de atsanız, Buzdolabına da (Eğer Cihazın içinde JRE- Java Sanal Makinesi Yüklüyse) sorunsuz ve hatasız çalışır!

...
**Ne İşe Yarar?**
* **Diller-Arası Kusursuz Özgürlük (Language Interoperability):** F# (Fonksiyonel Dil) ile bir "Matematik Motoru" (.dll) yazarsınız. O motor derlendiğinde .NET ara diline (IL) girmiş olur. Takım arkadaşınız Gidip C# (Nesne Yönelimli) Projesiyle sizin Matematik Engine'i sanki C# mış gibi Gözü Kapalı Çağırıp Kullanabilir! Çünkü "Firma Mülklerini (Classları) İngilizce'de (IL'de)" Birleştirmiştir.

## Dilin Mantığı ve Kod Yapısı
Tam bir Jargondur. Bytecode'un Aksine Okunması "İnsana Çok Daha Yakındır". Metinsel Bir Assembly'ye (ILDASM - IL Disassembler) benzer. 
İçinden Şaşaalı `class`, `public` gibi Yüksek Seviyeli Mimariler Akar; Bu yüzden "Tersine Mühendislikte (Crack/Hack'de) .NET Uygulamalarını Kırmak" (ILSpy / dnSpy programlarıyla) C++ Uygulamalarını Kırmaktan Kat VE Kat Daha Kolaydır! Adamın C# Kodunun Aynısı Geri Alınabilir (Decompile).

### Örnek Bir MSIL / CIL Kodu (C#'da Nasıl Yazılır, Neye Dönüşür?)
C# Dilinde Yazılan Klasik (Aşağıdaki) Ekrena "Selam" Basma Kodumuz:
`Console.WriteLine("Merhaba Dunya");`

Eğer C++ Derleyecisinden gexseydii 10 Milyon Satır Rezil Okunmasi Zor Assembly İnecekti.
Ancak .NET (IL) Derleyecisinden Çıkırtığığında (İçine girip Baktığımgızda); Efsanevi "Stack(Yüke/Çek) MAntıklı" Mükkemmel Anlasılır Bir Emir Zinzidirine (İL Makinesi) Donnusurrr:

```cil
// BU BIR MSIL / CIL (Ara Kod / Intermediate Language) KODUDUR :

// 1. CLASS VE FONKSIYOJN TANIMI (MSIL Obije Yonelimldiir DEDIK! Sakla R!!)
.class public auto ansi beforefieldinit BenimProgramim
{
  
  // 2. MAIN(GIRIS KAPISI) FUNKSIYINUNU INSA ET (Static Voild Main Yapisinin Aynsi)
  .method public hidebysig static void  Main() cil managed
  {
    .entrypoint        // (Baslatici EMIR BURAAASI!)
    .maxstack  8       // (Arka plandia 8 lik Bir Stack Hasfizesi AyuiriI!)
    
    // 3. EYLEM !! MATEMATK VE OPERASYONLAR
    // Ekrana(Masaya/Stack'e) 'Merhaba Dunya' Kelimesin İndir(Yukkle )
    IL_0000:  ldstr      "Merhaba Dunya"  
    
    // Call EMri İle .Nettin Kendi Çekirdek KÜütophaensşnden "WriteLine(Yzzi YAZdir)" i Cagiarark MAsdaki Keileyimi Bas! 
    IL_0005:  call       void [mscorlib]System.Console::WriteLine(string)
    
    // 4. KAPANIS (Return - Bitti)
    IL_000a:  ret
  } // End Of Main 

} // En of CLassss
```

Bu `ldstr` C# programcısının Kodu (exe) nun taa CİĞERİDİR! Microsoft bu IL katmanını o kadar muazzam Optimize etmiştir ki(.NET 8/.NET 9); C++'in Saf Derlemelerine (Nativi) Kafat Tutan hatta Geçecek Hızlaraa Çıkan Uçan Bir Motor (JIT) Ortaya Çıkartmıstı.


## Kimler Kullanır?
* Kendi Windows/Cihaz Uygulamalariı Yazılımının Performasnısnı Otrta Katamni Cok Çok Derinlemesine Profilleyen **Üst Düzay(Senior/Mimar) .NET Yazılımcıları.**
* Programda Virüs (Malware) Veya Hile Varmıdiye Kontrol EEdeen Arayuzlerii Decompile Eden (DnSpy) **Sİber Güvenlil (Cheteat Analistleri)**. Bu yüzden Çoğu C# Firması Kodlaırını SAlarken Araya "Obfuscator(Karıştırıcı/Bozucu)" koyar. Yoksa Rakipleri Kodlarii "IL Katmanidan" Cekip C#a GERi CVevirererek (Sourrce Kod Klonlayanarak) Emekleribnii Calbailrilril!

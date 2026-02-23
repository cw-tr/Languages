# C# (C-Sharp)

## Özet
C#, 2000'li yılların başlarında Microsoft tarafından doğrudan "Java Katili" olmak üzere tasarlanmış, çok yetenekli, modern, esnek ve .NET ekosisteminin mutlak kraliçesi olan Nesne Yönelimli Programlama (OOP) dilidir.

## Nedir ve Ne İşe Yarar?
1990'ların sonlarında Java Microsoft Windows'da büyük bir tekel olduğunda Microsoft kendi Java sürümünü çıkarttı (J++). Java (Sun Microsystems) Microsoft'a "dilimi çaldın" diyerek dava açtı ve kazandı. Microsoft da mecburen dâhi mühendis Anders Hejlsberg'i (Delphi ve TypeScript'in de yaratıcısı) işe alıp sıfırdan "bizim dilimiz" dediği C# dilini yarattı. 

Java'nın sanal makinesine alternatif olarak "Common Language Runtime (CLR / .NET)" adlı kendi muazzam işleyici motorunu kurdu.

**Ne İşe Yarar?**
* **Oyun Geliştirme (Unity):** Unity oyun motorunun tüm kodlama felsefesi C#'tır. C++'ın aksine oyun prototiplemek çok daha hızlı olduğu için Pokemon Go'dan Hearthstone'a kadar binlerce popüler mobil ve PC jenerasyonu oyunu C# mantığı kullanılarak tasarlanmıştır.
* **Masaüstü ve Kurumsal Web Sistemleri (.NET):** Tüm Windows programları (.exe dosyalarının arayüzlü WPF/WinForms olanları) native gibi C# ile yapılır. Kurumsal dev web sitelerinde (örneğin devasa E-Ticaret ve Stok sistemlerinin Backend sunucularında) .NET Core (ASP.NET) olarak Java'nın amansız tek rakibidir.
* **Çok Platformluluk:** Başta sadece Windows'ta çalışıyordu ancak bugün ".NET Core/5/6/7/8" sayesinde Microsoft dilini Linux ve Mac'te de çalışır, açık kaynaklı (Open Source) hale getirdi.

## Dilin Mantığı ve Kod Yapısı
Felsefesi Java'ya %90 oranında benzer. Yine "Sınıflar (Class)" zorunlu. Yine Çöp Toplayıcı (Garbage Collector). Yine güçlü tipler (Integer, Float, String gibi katı ayrımlar). 

Fakat Microsoft zaman içinde C#'ı o kadar hızlı modernleştirdi ki, dil Java'da olmayan (sonradan çalınan) çok pratik şekerlemelerle doldu. LINQ adındaki inanılmaz teknolojiyle kodun içine doğrudan SQL sorgusu gibi kelimeler yazıp veritabanını tarayabilir, "Async/Await" mimarisiyle kodu hiç dondurmadan arka planda iş yapabilirsiniz. Üstelik C++ özleyenler için "unsafe" tagi ile Java'da yasaklı olan "pointer" deliliğine bile girebilirler!

**Örnek İşleyiş (Sembolik Olarak):**
Java'daki gibi Getter/Setter adı altında 50 satırlık hantal (boiler-plate) özel özellik kodları yazmazdınız. C#'ta değişkenlerin (Properties) `get; set;` adında mikroskobik otomatik özellikleri vardır, tek satırda aynı güvenlik zırhını kurarsınız.

### Örnek Bir C# Kodu: LINQ Gücü ve OOP Zarafeti
Modern C#'ın sadece bir C veya Java türevi olmadığını, veri çekerken adeta "Sorgu Dili (SQL)" gibi davrandığı o meşhur LINQ yapısına ve sade sınıflara bakalım:

```csharp
using System;               // Standart kütüphane
using System.Linq;          // İhtişamlı LINQ (Veri Sorgulama) kütüphanesi
using System.Collections.Generic;

// Modern C# Sınıf (Class) tanımı (Kısacık Getter ve Setter'lar ile):
public class Oyuncu 
{
    public string Isim { get; set; }  // Değişkene doğrudan hem kapsülleme hem değer
    public int Skor { get; set; }     // Dışarıdan okunabilir ama güvenli
}

public class Program 
{
    // Programın Başlama Noktası (EntryPoint)
    public static void Main() 
    {
        // Bir sürü oyuncuyu Liste halinde (Objeler listesi) belleğe veriyoruz
        List<Oyuncu> oyuncular = new List<Oyuncu> {
            new Oyuncu { Isim = "Mehmet", Skor = 450 },
            new Oyuncu { Isim = "Ali", Skor = 1200 },
            new Oyuncu { Isim = "Ayse", Skor = 850 },
            new Oyuncu { Isim = "Zeynep", Skor = 3000 }
        };

        // İŞTE C# FARKI (LINQ MUCİZESİ):
        // Listede dön (for-loop) kurup "skoru 1000'den büyük olanları bul ve sırala" diyeceğine: 
        // Dümdüz Veritabanı sorgusu gibi "Sırala (OrderBy), Seç (Select)" komutlarıyla dilden filtre istersin.
        
        var sampiyonlar = oyuncular
                            .Where(o => o.Skor > 1000)            // Sadece skoru 1000'den büyük olan objeleri ayır
                            .OrderByDescending(o => o.Skor)       // Zirveden aşağıya (Yüksek->Düşük) doğru sırala
                            .Select(o => o.Isim);                 // Sadece isimlerini String olarak bana dön!

        Console.WriteLine("--- Turnuva Şampiyonları (1000 Puan Üstü) ---");
        
        // Yukarıdaki sihirli sorgudan dönen şampiyon isimlerini ekrana bas
        foreach (var isim in sampiyonlar) 
        {
            Console.WriteLine("-> Kazanan: " + isim);
        }
    }
}
```

## Kimler Kullanır?
* Evrendeki neredeyse tüm "Unity" Oyun motoru (Indie ve AAA) geliştiricileri; Mobil oyun, VR/AR, PC oyunu stüdyolarındaki Game Programmer'lar.
* Tüm büyük şirketlerdeki kurumsal yazılım (Backend) departmanlarındaki MVC veya API kodlayan Microsoft (.NET) ekosistem geliştiricileri. 
* Microsoft'un bulut devi Azure üzerindeki bulut-doğal (Cloud-native) sunucusuz işlemleri kurgulayan uzmanlar.

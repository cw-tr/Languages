# Vala

## Özet
Vala; 2006 yılında Jürg Billeter ve Raffaele Sandrini tarafından (GNOME projesi şemsiyesi altında) icat edilen, yazılımcılara **"Sanki C# veya Java yazıyormuş gibi"** rahat modern Sınıflar/Nesneler(OOP) yazdıran, ancak derlendiğinde (Compile) anında arka planda saniyeler içinde **Saf C Koduna dönüşen** harika ve akıllı bir Açık Kaynak transpile/sistem dilidir.

## Nedir ve Ne İşe Yarar?
Linux dünyasının en meşhur grafiksel arayüzlerinden biri olan GNOME ekranları (GTK+ arayüz serisi), tarih boyunca tamamen saf C dili ile ve "GObject (GLib)" denen korkunç ve manuel bir C-Nesneleştirme kütüphanesiyle yazıldı. 

Ekrana bir buton eklemek ve o butonu C diliyle "C aslında Obiject Oriented degildir" yalanını kandırıp GObject makrolarıyla Class'mış gibi göstermek için sayfalarca rezalet kod dökülüyordu. C++ veya Java kullanamazlardı çünkü C'nin GNOME C-Kütüphaneleriyle milisaniyelik Native entegrasyonundan vazgeçmek istemiyorlardı.

Vala ekibi; "Biz C# (Microsoft) dilinin sözdizimini (Syntax) direk kopyalayalım, C# o kadar rahat ve şık bir OOP dilidir ki geliştiriciler For/Class yazarken rahat etsin. Sonra o programı derlerken, biz onu arkaplanda gizlice **Açtıkları Class'ları Saf C (GObject) koduna** metin metin çeviririz, sonra da C derleyicisini(GCC) o çevrilen kodla çakar Saf makine Linux programına vururuz" dediler. Vala ortaya çıktı.

**Ne İşe Yarar?**
* **Elementary OS İşletim Sistemi:** Windows/Mac estetiğine sahip Elementary OS Linux dağıtımının içindeki tüm yerel uygulamalar (Hesap Makinesi, Takvim, Tarayıcılar), GNOME ortamında sıfır gecikmeyle çalışmak adına %100 Vala ile yazılmıştır.
* C dilinin Linux System/GTK kütüphanelerini C# rahatlığıyla sömürmek isteyen masaüstü geliştiricilerinin arka plan aracıdır.

## Dilin Mantığı ve Kod Yapısı
Tam bir "Ara Geçiş / Proxy" (Transpiled) dildir. Tıpkı TypeScript'in arka planda JavaScript'e dönüşmesi gibi, Vala dilinin kodları da C koduna döner! 

Sözdizimi aşırı derecede C# ve Java'ya benzer. Class'lar (Sınıflar), Interface'ler, Lambda (Ok, `=>`) fonksiyonları vardır. En büyük güzelliği ise Java/C#'taki JVM/CLR(Sanal Zayıf motorlar) olmadan, uygulamanın C derleyicisi (GCC) ile direkt **saf donanım C Hızında (.elf Linux exe formatında)** çalışmasıdır. Ancak bir Garbage Collector'u (Çöp Toplayıcısı) yoktur (!). Bunun yerine Apple/Swift veya Rust gibi "Otomatik Referans Sayımı (ARC - Automatic Reference Counting)" kullanarak hafıza silmeyi güvenli ve hızlıca kendi halleder.

**Örnek İşleyiş (Sembolik Olarak):**
Siz Vala kodu yazarsınız: `class Araba { public int Hiz; }`
Derleyici `valac`, arkadan onu alır ve şuna C dosyasına çevirir: `struct _Araba { int Hiz; ... }; void araba_yap(Araba* s);` (Bütün GObject C makrolarını sizin yerinize döşer ve gizler).

### Örnek Bir Vala Kodu: C#'tan Ayırt Edilemeyen "C / GTK Uygulaması"
Linux grafik ekranı (GTK) kütüphanesini sanki C# Windows Form Applications kodluyormuşcasına Pürüssüz OOP Sınıflarına (Signals/Events) hapseden harika bir GNOME Arayüz kodu:

```vala
// Vala dilinde yorum satirlari C# / C dilleri gibidir (// ve /*)

// Linux GNOME Grafik Arayuzunu Iceri Aktar(Using/import misali)
using Gtk;

// TAMAMEN C# ANDIRAN OOP MİMARİSİ
// "Pencerem" Sınıfı(Class), bir Gtk.Window eklentisinden (Classindan) Kalitim(Miras) alir.
public class Pencerem : Window {
    
    // Class'in Constructor'i (Kurucusu)
    public Pencerem () {
        
        // Window Klasina (C#'da base() mantigi) özellik bas:
        this.title = "Vala ile Linux Arayüzü!";
        this.set_default_size (300, 200);
        this.border_width = 25;
        this.window_position = WindowPosition.CENTER;
        
        // Ekrana Şık Bir Buton Objtesi Yaratiyoruz
        var butonObjesi = new Button.with_label ("Beni Tikla Coder!");

        // LINUX GTK SIGNAL(Event/Olay MİMARİSİ) ve LAMBDA FUNKSİYONLAR 
        // Sanki C# içindeki delegate / event'lar gibi yazılır
        butonObjesi.clicked.connect (() => {
            
            butonObjesi.label = "Tebrikler Vala Kodunu Oynattin!";
            
            // Console'a arka plan da yazi firlat:
            print ("Konsol Logu: Butonuna Basildi C'ye Sinyal Gitti.\n");
        });
        
        // Butonu Pencerenin kalbine ekle
        this.add (butonObjesi);
        
        // Gtk C-Makrosu olan "destroy/quit" sinyalini tek tusa bağla (Pencereyi carpidan kapatinca program bitsin)
        this.destroy.connect (Gtk.main_quit);
    }
}

// PROGRAMIN CALISMA MERKEZI MİMARİSI (MAIN)
static int main (string[] args) {
    
    // Gtk Grafik Arayuz Motorunu Cihazda/Linuxda atesle (Init)
    Gtk.init (ref args);
    
    // Objemizi Hafizada (Reference Counting ile) Yarat;
    var uygulama_ekrani = new Pencerem ();
    
    // Her seyi cizdir ve goster
    uygulama_ekrani.show_all ();
    
    // İşletim sistemini Bekleme Döngüsüne (Event Loop) Sok:
    Gtk.main ();
    
    return 0; // Sorunsuz bitti 
}
```

Bu döküm, Terminalden `valac --pkg gtk+-3.0 dosyam.vala` denildiği an kapalı kapılar ardında koca bir C kod yığınına dönüşür, ve gcc tarafından native sistem kompsitesine makine kodu olarak patlatılır.

## Kimler Kullanır?
* Evrendeki sadece **Linux Dağıtımcıları (Özellikle GTK/GNOME Grafik arayüzü masaüstü projeleri kodu mimarları)**. 
* C++ veya Rust öğrenmek istemeyip; ama C/Java'nın hantallığı olmadan da Linux masaüstüne 0.1 milisaniyede açılan "Hafif (Lightweight)" takvimler, hesap makineleri, klasör (File manager) programları ve FlatPak sistemlerini kurmak isteyen Hızlı-Linux Geliştiricileri. 
* C#'ın Windows tahtında sağladığı RAD rahatlığını, kendi Linux GTK dünyalarına çekip adapte olmak isteyen Start-up tabanlı kitleler.

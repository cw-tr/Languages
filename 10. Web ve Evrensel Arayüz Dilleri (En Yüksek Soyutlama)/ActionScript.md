# ActionScript

## Özet
ActionScript; 1990'ların sonunda Macromedia firması tarafından(Daha sonra dev Adobe tarafından satın alınan) **Flash Player (Animasyon)** ekosistemine zeka ve etkileşim (Interaction) katmak için icat edilen, 2000-2010 arası bütün Web (Browser) Flash Oyunlarının, Animasyonların ve Videoların (YouTube ilk yılları) "Tıklama, Zıplama, Koşma" kodlarının yazıldığı Dünyanın Eski Nesil En Popüler **Etkileşim/Animasyon Dili** kurgusudur.

## Nedir ve Ne İşe Yarar?
Eskiden (1998-2005) web siteleri dümdüz HTML metinlerinden ibaretti. Javascript aşırı yavaştı ve güçsüzdü. Ekranda zıplayan bir Top, patlayan bir havai fişek ya da tarayıcıda akan bir Mario oynamak istiyorsanız **Adobe Flash Player** adlı eklentiyi (Plugin) bilgisayarınıza kurmak zorundaydınız. 

Flash programında grafikerler Çizim Çiziyor (Timeline'a keyframe atıyordu). Ama o karakterin *Geri Tuşuna* basınca yürümesi, Ateş tuşuyla kurşun atıp Skoru +1 yapması için O Çizimin içine KOD GÖMMEK gerekiyordu. İşte Flash Çizimlerine Can veren o dilin adı: **ActionScript** idi. 

**Ne İşe Yarar?**
* **Flash Web Oyunları:** Miniclip, KralOyun gibi sitelerdeki eski milyonlarca 2D Zıplama/Macera ve Strateji Flash web oyunlarının tüm fizik çekirdeği ve UI aksiyonları ActionScript 2.0 veya 3.0 ile yazılmıştır.
* **Flash Animasyon ve Eğitim Slaytları:** Tıklandığında sayfa değiştiren, şifre ile girilen E-Öğrenme (E-learning) veya kurumsal tanıtım CD/Swf animasyonlarının buton otomasyonları.
* **Ses ve Video Kontrolü:** O dönemki YouTube'un "Durdur / Oynat / Sesi Aç" butonları tamamen ActionScript mantığıyla, C++ Flash eklentisini komutluyordu.

## Dilin Mantığı ve Kod Yapısı
Tam bir **ECMAScript (JavaScript Kardeşi)** klonudur! Zaten kökeni, JavaScript'in kurallarına bağlıdır. Lakin Apple'ın ve HTML5'in Flash'a açtığı savaş yüzünden, ActionScript (Özellikle ActionScript 3.0 ile) kendini tamamen **Saf, Ciddi, Strict Typeli Nesne Yönelimli Bir Gelişmiş Dile (OOP Java/C# formuna)** çevirmiş, asil bir "Arayüz Dili" olmuştur.

AS3'te (ActionScript 3), Tip atamaları İki Nokta Üst üste (`var yas:int = 5`) ile deklare edilir (ki TypeScript aslında bu yazım tarzını ActionScript'ten arakalayarak dünyada ünlü olmuştur!).  Güçlü "Display List (Ekran Objeleri Listesi)" sistemiyle Ekrana Grafik Ekler (`addChild`) veya çıkarır (`removeChild`).

**Örnek İşleyiş (Sembolik Olarak):**
Butona tıklandığında bir iş yapmak!
ActionScript 3 : `oynaButonu.addEventListener(MouseEvent.CLICK, oyunuBaslat);` (Sonsuz derecede dinleyici/event ekle).

### Örnek Bir ActionScript 3.0 (AS3) Kodu: Flash Karakterini Faremizle Tıklayıp Zıplatmak!
Animasyon zaman çizelgesi (Timeline) arkasındaki 1. Frame'e (Kareye) yazılmış, OOP Ciddiyetindeki o meşhur Zeka Motoru:

```actionscript
// ActionScript, JS ve C++ tabanli cift tire (//) Veya /* */ yorum bloklari kullanir

// Eger Buyuk Bir Proje Yapiyorsak (Package ve Import zorunludur)
// Flash'in Görsel (Display) ve Olay (Event) kutuphanelerini Cagir:
package komutlarim {
    import flash.display.MovieClip;
    import flash.events.MouseEvent;
    import flash.events.Event;

    // ANA CLASS(Sınıf) - "OyunZemini" Bir Animasyon/Film Karesidir (Movieclip'ten kalitim yap)
    public class OyunZemini extends MovieClip {
        
        // DEGISKEN (TIPI DIKKAT! ': int' yazarak TYPESCRIPT gibi Ciddi Tip mühürleme)
        private var ziplamaGucu:int = -15; 
        private var yerCekimi:Number = 2.5;
        private var karakterHiziY:Number = 0; // Yukari/Asagi (Y Eksen) ivmesi

        // CONSTRUCTOR (Otomatik Kurucu, Flash Ekrana Geldıginde Uyanir):
        public function OyunZemini() {
            
            // Ekranda cizili "sahnedeki_karakter" isimli kutuya Dinleyici Tıkla kancasi tak:
            sahnedeki_karakter.buttonMode = true; // Uzerine gelince fare el olsun!
            
            // FARE KANCASI
            sahnedeki_karakter.addEventListener(MouseEvent.MOUSE_DOWN, ziplatSinyaliVER);
            
            // ZAMANLAYICI (Her 1 Saniyede 60 kere / 60 FPS Animasyon Tetikleyici Ruhu! ENTER_FRAME)
            stage.addEventListener(Event.ENTER_FRAME, herKaredeFizikUygula);
        }

        // FARE TIKLANDIGINDA CALISAN EVENT (VOID):
        private function ziplatSinyaliVER(mesaj:MouseEvent):void {
            
            // Karakterin Yukariya Dogru Cekilis ivmesini Pompala (- Yonde):
            karakterHiziY = ziplamaGucu;
            
            // Console (IDE) ekranina yazi firlat:
            trace("Ucuyoruz Kaptan!"); 
        }

        // HER SANIYE (60 Kere) MOTOR GIBI DONEN FIZIK HESAPLAMASI (GRAVITY YASALARI)
        private function herKaredeFizikUygula(zaman:Event):void {
            
            // Yercekimini surekli (Toplama) asagi Pompala:
            karakterHiziY += yerCekimi;
            
            // Karakteri Ekranda (Y_Ekseni Pikseli olarak) Oyle ciz (Hareket Ettir!)
            sahnedeki_karakter.y += karakterHiziY;
            
            // Eger Karakter ekranin Dibine Vurduysa (Orn: 500 ncu Pikselde) Duraklama yap:
            if (sahnedeki_karakter.y > 500) {
                sahnedeki_karakter.y = 500; // Cukura Dustu Kaldi
                karakterHiziY = 0; // Hiz Puanini Sifirla
            }
        }
    }
}
```

Bu kod derlendiğinde (Compile edildiğinde, Flex veya Adobe Animate üzerinde), ortaya O Meşhur `.SWF (Shockwave Flash)` formatı çıkardı. İnsanlar onu browsera atıp oynardı ve kimse arkaplandaki bu katı OOP matematik formüllerini görmezdi.

## Kimler Kullanır?
* Evrendeki bütün Flash Web Tasarımcıları, **Miniclip/Armorgames tarzı Flash Geliştiricileri** (Özellikle 2000'lerin altın web döneminde).
* Sonradan mobil iOS ve Android uygulaması çıkartmasına (Adobe AIR) izin verilmesine rağmen, HTML5'in Canvas özellikleri (ve Javascript gücü) ortaya çıkınca; ve o efsanevi olay yani Steve Jobs ("Flash'ı asla iPhone kapalı kutusuna sokmayacağım, çünkü aşırı sarj yiyor" dediği fermanı) üzerine; ActionScript Hızla çöktü.
* **GÜNÜMÜZDE; TAMAMEN ÖLÜ BİR MİRAS(Legacy) DİLİDİR.** 2020 Yılında Google Chrome dahil tüm Browserlar "Flash Player" motorunu kökünden (Güvenlik zafiyetleri sebebiyle) Tarayıcıdan bloke etti/kaldırdı. Artık web sitelerinde ActionScript oyunları çalışamaz; yerini tamamen JavaScript/HTML5 ve WebAssembly (Wasm) devralmıştır.

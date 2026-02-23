# Unreal Blueprints (Görsel Betikleme)

## Özet
Unreal Blueprints (MaviKopya); Epic Games firmasının Unreal Engine (Oyun Motoru) içerisine gömdüğü, Yüzlerce Satır İngilizce Kelime / C++ Kodu yazmak (Daktilo Etmek) yerine; **Kutucukları (Düğümleri - Nodes) Fareyle (Mouse) Birbirine Renkli Kablolarla Bağlayarak** "Aynı C++ Hızında ve Mantığında" Oyunlar, Yapay Zekalar, Mekanikler Kodlayabildiğiniz Dünyanın EN GELİŞMİŞ ve En Popüler **Görsel Betikleme (Visual Scripting) Dilidir/Sistemidir**.  

*(O Metin tabanlı Bir dil değildir. Fakat Derlenişi, Mantığı Turing-Complete(Tam Eksiksiz) bir C++ Makinesidir).*

## Nedir ve Ne İşe Yarar?
10 Yıl önce Trilyon Grafiklere sahip Unreal Engine Motorunda oyun Yapıyorsanız; Karakteri Yürütmek İçin (Visual Studio açıp) C++ dilinin O korkunç Derecede Yüksek-Seviyeli Kütüphaneleriyle Pointer ve Hafıza yönetimi eziyetine giriyordunuz. Oyun Tasarımcıları (Sanatçılar, Animatörler) Yazılımcılara Yalvarıyordu: "Lütfen kapı C Tuşuna basınca açılsın!".

Epic Games Dediki: **Kodu Sileyim! Sanatçıların ve Geliştiricilerin Anlayacağı Akış-Şemasına (Flowchart) Çevireyim!**
Kütüphanedeki C++ Fonksiyonlarını (Örneğin: `DestroyActor()`) Alıp, Kırmızı Çerçeveli Kutu (Node) Olarak Motorun içine Ekledi. Siz Farenizle "Tetikleme İpini (Exec Pin)" Sürükleyerek "Ekranda Oyuncu Ölürse ---> İp ---> Aktörü Yok Et!" diyerek Görsel bir İplik ağ örersiniz.  Ve Şaşırtıcı Şekilde; O Çizdiğiniz Tablo Arkaplanda **C++'a Çevrilip (Nativize Edilerek)** Ekrana Basılır!

**Ne İşe Yarar?**
* **AAA Oyun Stüdyoları (Epic Games Kalitesi):** Fortnite, Tekken 8, Hogwarts Legacy, Jedi: Survivor... Bütün bu Milyar dolarlık devasa oyunların **Tasarımcıları / Bölüm Yaratıcıları (Level Designers)** C++ Bilmez. Onlar Tamamen Blueprints İle Düşmanların Nerde Çıkacağını, Kapıların Nasıl Kilitleneceğini Örümcek Ağı Gibi Çizerler!
* **Kodlama Bilmeden Yazılımcı(Geliştirici) Olmak:** Syntax error (Noktalıyı Virgülü unuttum) Derdi Yoktur. Hata yaparsanız Kırmızı Çizgi Kopar.  

## Dilin Mantığı ve Kod Yapısı
Klavye Yoktur. Tamamen Bağlanılabilir (Pin) Kutu Arayüzüdür.
Akış Renge Göre İşler:
1. **Beyaz (Execution/Çalıştırma) Kablosu:** Kalp Atışıdır. Zamanı/Sırayı Temsil eder. Eğer Düğmenin Beyaz kablosunu Akıtmıyorsanız O Kod Asla Çalışmaz. (C++ taki Satır altlarına tekabül Eder).
2. **Kırmızı Kutu (Event/Olay Düğümü):** Başlatıcı Tetiklerdir. *Oyun Başladığında*, *Adama Mermi Değdiğinde* (Event Tick, Event BeginPlay).
3. **Değişken Renkleri (Hafızalar):** 
   - Kırmızı İp: Boolean (Doğru/Yanlış)
   - Yeşil İp: Float (Ondalıklı Matematik / Hasar Miktarı)
   - Camgöbeği (Cyan): Integer (Tam Sayılar - Cephane sayısı)
   - Mavi İp: Obje / Aktör Eşlemesi.

### Örnek Bir Blueprint Akış Semantiği: (Can Azalma Mimarisi)
Eğer C++ (veya C#) ile Klasik kod yazsaydık:
```cpp
// Eger Ates Edilmis İse
if(Mermi_Carpti_Mi) {
    Can = Can - 20.0f;           // Canı Azalt
    if(Can <= 0) { Die(); }      // Eger Öldüyse YokEt fonksiyonu cagir
}
```
**Bunun Blueprint'te Masadaki Çizimi Şöyledir:** (Ekran Okuyucu Temsili)
1. Ekrana Kırmızı Bir Kutu Konur: **[Event Hit (Bana Çarptılar)]**
2. Bu kutudan Çıkan BEYAZ İP(Kablo) farenin sol tuşuyla çekilerek Siyah Gürültülü Bir Kutuya Götürülür: **[Branch (Dallanma / İf Şartı)]**
3. Oyuncunun Sağlık (Can) Float(Yeşil) rakamı(100) Ekrana Sürüklenir. Ondan 20 Cıkartan Bir Matematik Kutusu **[ - Subtract]** na Bağlanır. Sayı 80 kalır.
4. Çıkan 80 rakamı Küçük Müdür Sıfırdan `<= 0 Kutusu` na Bağlanır.
5. Matematik Doğruysa (True), Beyaz ip  **[Destroy Actor Kutusu]** 'na Takılır. Oyuncu Ekranda Parçlalanır Patlar (Yok Olur). Bütün Bunları Yazı okumadan, Sol beyin Sağ Beyin Entegrasyonuyla Lego Oynar gibi Tasarlarsınız. 

## Kimler Kullanır?
* C++ Yazılımcılarının Kurduğu Devasa(Milyon Satırlık) Sistemleri alıp, Eğlenceli "Görev (Quest) Sistemlerine", "Silah Mermilerine", "Pencere UI Animasyonlarına" dönüştüren **Gameplay (Oyun Oynanış) Tasarımcıları**.
* Hayatında Tek Satır Kod Görmeden Unreal Engine indirerek kendi Başlarına Mükemmel FPS Piyasası/Korku(Outlast gibi) oyunlar yapan ve Satış rekoru  Kıran Bağımsız Yapımcılar. 
* *Blueprint Spagettisi:* Kötü Mimar olanlar o kadar çok Kablo çekerler ki, Ekran Mavi ve kırmızı renkle Dolanmış İğrenç Bir Spagetti(Kablo Çorbası) Makarnasına döner (Programlamadaki Spagetti kodun Fiziksel Enjeksiyonudur). Lakin günümüzde Dünya Grafik ve Simulasyonlarının Kalbi Görsel Programlamadan Geçmektedir.

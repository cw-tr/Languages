# Lua

## Özet
Lua (Portekizce "Ay"); 1993 yılında Brezilya'da (Pontifical Catholic University) geliştirilen, dünyanın **en hafif, en hızlı, en gömülebilir (Embeddable)** ve yazılım/oyun endüstrisinin arkasındaki "gizli süper kahraman" olan mikroskobik boyutta fakat muazzam güçlü bir betik (Scripting) dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda C/C++ ile devasa bir yazılım veya oyun motoru yazdığınızda, eğer küçük bir konfigürasyon (Örn: Arabanın hızını 50'den 60'a çıkarmak) yapmak isterseniz bütün o 2 milyon satırlık projeyi baştan derlemeniz gerekiyordu ve bu saatler sürüyordu. 

Brezilyalı ekip, "Biz C dilinin veya Java'nın tam kalbine gömülebilecek, kilobaytlarca (şu an ~300 KB) küçüklükte, ama programın içindeyken anında dışarıdan C dilinin kalbindeki fonksiyonları değiştirebilen minik bir parazit dil yazalım" dedi. Bu yüzden o devasa diller "Dünya" ise, bu etrafında dönen o minik dilin adı "Ay (Lua)" oldu.

**Ne İşe Yarar?**
* **Oyun Geliştirme (Gizli Asker):** World of Warcraft (WoW)'daki eklentilerinizin arayüzü, Roblox'un oyun tasarım dili, Angry Birds'ün o devasa fizik motorunun kuş fırlatma parametreleri, Grand Theft Auto (GTA) V içindeki sahneler/görevler. Bütün bu dev oyun motorları C++ ile yazılmıştır ancak oyunun "akışı/hikayesi/bölümleri" asla baştan derlenmesin diye C++'ın içine ufak .lua dosyaları olarak gömülmüştür!
* **Gömülü Sistemler ve Araçlar:** Photoshop'dan Adobe Lightroom'a (Presets), Nginx sunucusundan Redis veritabanına, Wireshark ağ analizöründen VLC oynatıcısına kadar binlerce masaüstü C++/.exe programının arkasında, kullanıcıya "Kendi eklentilerini yazma" (Plugin) şansı vermek için içine C/C++ API'siyle gömülmüş olan tek bir komut modülü (Lua Interpreter) yatar.

## Dilin Mantığı ve Kod Yapısı
Çok ama çok basittir, öğrenmesi Python'dan bile kolay bir "Glue Language (Yapıştırıcı Dil)"dir.

En önemli veri mimarisi (hatta tek karmaşık veri mimarisi) **Tablolar (Tables)** dır. Aslında Lua'da Liste(Array), Sözlük(Dictionary/Hash), Sınıf(Class), Modül (Package) diye kavramlar KESİNLİKLE YOKTUR. Bütün bu kavramların hepsi, sihirli bir yapı olan "Tablo `{}`" iskeletiyle yapılır.

C/C++ dilleri indeksleri (sıralamayı) `0` ile başlatırken "İnsan gibi düşünsün" felsefesi gereği Lua (Tıpkı R veya MATLAB gibi) indekslemeye (Saymaya) daima `1`'den başlar.

**Örnek İşleyiş (Sembolik Olarak):**
Siz bir C++ uygulamasında (Oyun motorunda) `Kahraman.zipla()` diye fonksiyon yazarsınız. Ancak zıplamanın gücünü belirlemek için C kodlarına `zip_guc = 5` yazıp saatlerce derlemek eziyettir. Yan odaya bir `ayarlar.lua` dosyası açar, C makinesine "Git bu LUA ayar dosyasını oku, her zıplamadan önce" dersiniz. Siz oyun oynarken ve exe AKTİF ÇALIŞIRKEN arkadan ayarlar.lua'ya girip Not defteriyle sayıyı 20 yapar Kaydet dersiniz, karakter anında C hızıyla havaya uçar, derlemeye (Lag girmeye) gerek kalmaz.

### Örnek Bir Lua Kodu: Tablo Mucizesi (Sınıf, Dizi ve Objenin Birleşimi)
Lua'da özel `Class` vs gibi eziyetlerin olmaması, sadece süslü tablolarlar her şeyin inşa edilmesi ve OOP/Döngü simülasyonu:

```lua
-- Lua Dilinde yorum satirlari cift tire '--' ile gosterilir.

-- Lua'nin yegane MUCİZESİ (Sihirli Tablo): 
-- İçine hem Dizi(Array), hem Degisken, Hem İsimli Anahtar ve Hem FONKSIYON alan Matrix.
-- (Bunun içinden oyun motorundaki C/C++ verisine mudahale edicez).

OyunAyarTablosi = {
    oyuncu_ismi = "Kovboy",       -- Key/Value Mantiği
    cani = 100,
    envanter = {"Silah", "İp", "At"}, -- Normal Dizi Mantiği (İç İçe array)
    
    -- Fonksiyonu (Metodu) bile bu tablonun icine C# Objesiymiscesine tikabiliyoruz!
    hasar_al = function(self, miktar) 
        self.cani = self.cani - miktar
        print(self.oyuncu_ismi .. " Hasar yedi! Kalan Can: " .. self.cani)
        -- Not: ".." isareti İki String Metni yan yana yapistirir (Birlestirme).
    end
}

-- Çagirma veya Okuma (Lua Programin arkasinda sessizce yorumlayicisindan saniyede gecer):
print("Oyun Yuklendi: HOSGELDIN " .. OyunAyarTablosi.oyuncu_ismi)

-- Fonksiyonu Calistiriyoruz ( ':' (iki nokta) sözdizimi "self" yerine parametre kisaltmasidir):
OyunAyarTablosi:hasar_al(25)


-- === LUA MANTIGI (INDEKSLER 1'DEN BASLAR) ===
print("\n--- Envanter ---")

-- pairs (Tablo Anahtari ceker) ve ipairs (Sirali Dizi Endeksi çeker):
for i, esya in ipairs(OyunAyarTablosi.envanter) do
    
    -- i (Index C++'in aksine Sifir(0) değil, BİR(1)'den başlar!):
    print("Esya No: " .. i .. " - İsim: " .. esya)
    
end
```

Bu kod C++ motoruna derlenmiş "Lua.h" kütüphanesinden sokulduğu an, oyunu kapatmadan karakter canı hesaplatan o meşhur gömülü (embedded) süspansiyona dönüşür. 

## Kimler Kullanır?
* Evrendeki Game-Engine (Oyun Motoru - Unity demiyoruz, özel yapım C++ AAA Oyun motorları) mimarları, Roblox oyunu (Eğlence platformu) içindeki tasarım / seviye modlayıcı (Modder) toplulukları.
* Ağ/Trafik Gözlemcileri (Cisco, Wireshark, Nmap vb.) içinde hız testini kesmeden (sistemi baştan derlemeden) hızlı kalkan prototipi ayarlayan Network güvenlikçileri ve IoT (Sınırlı Hafızalı Gömülü Çip) Tost makinesi/Buzdolabı programlayıcıları (Sadece 300KB RAM ister).
